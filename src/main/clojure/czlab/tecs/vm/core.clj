;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.vm.core

  (:require [czlab.tecs.vm.fline :as fl]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:use [clojure.walk])

  (:import [java.util.concurrent.atomic AtomicInteger]
           [czlab.basal.core GenericMutable]
           [java.io File LineNumberReader]
           [java.net URL]
           [java.util Map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;RAM addresses
;;--------------
;0-15          Sixteen virtual registers, usage described below
;16-255        Static variables (of all the VM functions in the VM program)
;256–2047      Stack
;2048–16483    Heap (used to store objects and arrays)
;16384–24575   Memory mapped I/O
;;Registers
;;---------
;RAM[0]     SP Stack pointer: points to the next topmost location in the stack;
;RAM[1]     LCL Points to the base of the current VM function’s local segment;
;RAM[2]     ARG Points to the base of the current VM function’s argument segment;
;RAM[3]     THIS Points to the base of the current this segment (within the heap);
;RAM[4]     THAT Points to the base of the current that segment (within the heap);
;RAM[5–12]  Holds the contents of the temp segment;
;RAM[13–15] Can be used by the VM implementation as general- purpose registers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private decl-smxxx "" [s]
  `(def ~(with-meta
           (symbol (str "sm-" s))
           {:private true :tag String}) ~s))

;;dont change the order inside the array!
(def ^:private _segs_ ["argument" "local" "static"
                       "constant" "this" "that" "pointer" "temp"])
(def ^:private _seglocs_ [2 1 16 0 3 4 0 5])

;;def some constants
(def ^:private _SEGS_ (merge (c/preduce<map>
                               #(assoc! %1 %2 (s/ucase %2)) _segs_)
                             {"argument" "ARG" "local" "LCL" }))
(def ^:private _SMAS_ (zipmap _segs_ _seglocs_))
(def ^:private _counter (AtomicInteger.))

(defmacro ^:private decl-smxxx** []
  (let [ss (mapv #(str %) _segs_)]
    `(do ~@(map (fn [a]
                  `(decl-smxxx ~a)) ss))))
(decl-smxxx**)
(def ^:private sm-args sm-argument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private csj "" [args] `(cs/join "\n" ~args))
(defmacro ^:private pc! "" [x] `(str "(" ~x ")"))
(defmacro ^:private at! "" [x] `(str "@" ~x))

(def ^:private LCL! (at! (_SEGS_ sm-local)))
(def ^:private ARG! (at! (_SEGS_ sm-args)))
(def ^:private THIS! (at! (_SEGS_ sm-this)))
(def ^:private THAT! (at! (_SEGS_ sm-that)))
(def ^:private R15! (at! "R15"))
(def ^:private R14! (at! "R14"))
(def ^:private R13! (at! "R13"))
(def ^:private SP! (at! "SP"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- incSP "" [] (csj [SP! "M=M+1" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- decSP "" [] (csj [SP! "M=M-1" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pushSPWithD "" [] (str (csj [SP! "A=M" "M=D" ""]) (incSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextAutoLabel "" [pf]
  (str pf "." (c/jid<>) "." (.getAndIncrement ^AtomicInteger _counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass1 "" [ctx furl]
  (with-open [inp (-> (.openStream ^URL furl)
                      io/reader
                      LineNumberReader. )]
    (loop [total []
           cur 1
           rdr inp
           line (.readLine rdr)]
      (if (nil? line)
        (doto total)
        (let [fl (fl/fileLine<> cur line)]
          (recur (if-not (:blank? fl)
                   (conj total fl) total)
                 (inc cur)
                 rdr
                 (.readLine rdr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- thisthat<n> "" [offset] (get _SMAS_
                                    (if (= 0 offset) sm-this sm-that)))
(defn- thisthat<s> "" [offset] (get _SEGS_
                                    (if (= 0 offset) sm-this sm-that)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- remapOffset?? "" [ctx seg offset]
  (condp = seg
    sm-static
    (+ (get _SMAS_ seg)
       (let [{:keys [staticOffset fname vmvars]} @ctx
             vn (str fname "." offset)]
         (if-not (c/in? vmvars vn)
           (c/copy* ctx {:vmvars (assoc vmvars vn staticOffset)
                         :staticOffset (inc staticOffset)}))
         (get (:vmvars @ctx) vn)))
    sm-pointer
    (thisthat<n> offset)
    sm-temp
    (+ (get _SMAS_ seg) offset)
    offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handlePush "" [ctx segment offset]
  ;;1. value from [seg+offset] => D-Reg
  ;;2. we have the target data in D-REG
  ;;   set value to current stack pointer [sptr] = D
  ;;   then inc the stack pointer by 1
  (let [seg (s/lcase segment)
        offset (remapOffset?? ctx seg offset)
        out (cond
              (= seg sm-constant)
              (csj [(at! offset) "D=A" ""])
              (or (= seg sm-pointer)
                  (= seg sm-temp)
                  (= seg sm-static))
              (csj [(at! offset) "D=M" ""])
              (or (= seg sm-args)
                  (= seg sm-this)
                  (= seg sm-that)
                  (= seg sm-local))
              (csj [(at! offset)
                    "D=A"
                    (at! (_SEGS_ seg)) "A=M+D" "D=M" ""])
              :else
              (c/throwBadData "segment: %s?" segment))]
    (str out (pushSPWithD) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setValInR13ToAddrInR14 "" []
  (csj [(at! "R13") "D=M" (at! "R14") "A=M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popSPWithR13 "" []
  (str (csj [SP!
             "A=M-1" "D=M"
             (at! "R13") "M=D" ""]) (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setR14AsAddrInD "" [lbl offset]
  (str (csj (if (s/nichts? lbl)
              [(at! offset) "D=A" ""]
              [(at! offset) "D=A" (at! lbl) "D=D+M" ""]))
       (csj [(at! "R14") "M=D" ""])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handlePop "" [ctx segment offset]
  ;; pop value off the stack and
  ;; set value to the [ segment+offset ]
  ;; we store the target RAM address int R14 first
  ;; once we get the value off the stack,
  ;; then set the value into the target address
  (let [seg (s/lcase segment)
        offset (remapOffset?? ctx seg offset)
        out (cond
              (= seg sm-constant)
              (c/throwBadData "pop constant segment!")
              (or (= seg sm-pointer)
                  (= seg sm-temp)
                  (= seg sm-static))
              (setR14AsAddrInD "" offset)
              (or (= seg sm-local)
                  (= seg sm-args)
                  (= seg sm-this)
                  (= seg sm-that))
              (setR14AsAddrInD (_SEGS_ seg) offset)
              :else
              (c/throwBadData "segment: %s?" segment))]
    (str (popSPWithR13) out (setValInR13ToAddrInR14))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAdd "" [ctx]
  (csj [SP! "A=M-1" "D=M"
        SP! "A=M-1" "A=A-1" "M=D+M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleSub "" [ctx]
  (csj [SP! "A=M-1" "D=M"
        SP! "A=M-1" "A=A-1" "M=M-D" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAnd "" [ctx]
  (csj [SP! "A=M-1" "D=M"
        SP! "A=M-1" "A=A-1" "M=D&M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleOr "" [ctx]
  (csj [SP! "A=M-1" "D=M"
        SP! "A=M-1" "A=A-1" "M=D|M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_0 "" []
  ;; D has the value of x-y
  (csj [SP! "A=M-1" "D=M"
        SP! "A=M-1" "A=A-1" "D=M-D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_1 "" [cp t f]
  (csj [(at! t) (str "D;" cp) (at! f) "0;JMP" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_toggles "" [t f z]
  (csj [(pc! t)
        SP!
        "A=M-1"
        "A=A-1"
        "M=-1"
        (at! z)
        "0;JMP"
        (pc! f)
        SP!
        "A=M-1"
        "A=A-1"
        "M=0"
        (at! z)
        "0;JMP"
        (pc! z)
        (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleEQ "" [ctx]
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JEQ" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleGT "" [ctx]
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JGT" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleLT "" [ctx]
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JLT" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleNeg "" [ctx] (csj [SP! "A=M-1" "D=-M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleNot "" [ctx] (csj [SP! "A=M-1" "D=!M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleLabelDef "" [ctx lbl] (str (pc! lbl) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleIfGoto "" [ctx lbl]
  (str (popSPWithR13)
       (csj [(at! "R13") "D=M" (at! lbl) "D;JNE" ""])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleGoto "" [ctx lbl] (csj [(at! lbl) "0;JMP" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFunc "" [ctx func args]
  (let [out (s/strbf<> (str (pc! func ) "\n"))
        s (s/strbf<>)]
    (c/setf! ctx :func func)
    ;; deal with local vars ,
    ;; set them to zero, up the SP too, yes ?
    (doseq [i (range args)]
      (if (= i 0)
        (s/sb+ s (csj ["" (at! (_SEGS_ sm-local)) "D=M" ""]))
        (s/sb+ s "D=D+1\n"))
      (s/sb+ s (csj ["A=D" "M=0" ""])))
    (s/sb+ out s)
    (.setLength s 0)
    ;; adjust SP to cater for added local vars
    (doseq [i (range args)]
      (if (= 0 i)
        (s/sb+ s (csj ["" SP! "D=M" ""])))
      (s/sb+ s "D=D+1\n"))
    (if (pos? (.length s))
      (s/sb+ s (csj [SP! "M=D" ""])))
    (str out s "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFuncReturn "" [ctx]
  (csj ["//start the return nightmare...."
        (at! (_SEGS_ sm-local)) "D=M" (at! "R13")
        "M=D // [13] => LCL -> frame"
        (at! "R13") "D=M-1"
        "D=D-1" "D=D-1" "D=D-1" "A=D-1" "D=M"
        (at! "R14") "M=D // [14] -> return addr"
        ;; move the SP back to old place, also place the return value there
        (at! (_SEGS_ sm-args)) "D=M" (at! "R15") "M=D"
        SP! "A=M-1" "D=M" (at! "R15") "A=M" "M=D"
        (at! "R15") "D=M" "D=D+1" SP! "M=D"
        (at! "R13") "A=M-1" "D=M" (at! (_SEGS_ sm-that))
        "M=D // reset THAT"
        (at! "R13") "D=M-1" "A=D-1" "D=M" (at! (_SEGS_ sm-this))
        "M=D  // reset THIS"
        (at! "R13") "D=M-1" "D=D-1" "A=D-1" "D=M" (at! (_SEGS_ sm-args))
        "M=D // reset ARG"
        (at! "R13") "D=M-1" "D=D-1" "D=D-1" "A=D-1" "D=M" (at! (_SEGS_ sm-local))
        "M=D // reset LCL"
        (at! "R14") "A=M"
        "0;JMP // return code"
        ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleCall "" [ctx func args]
  (let [ret_addr (nextAutoLabel "fc")
        s (s/strbf<>)
        out (csj ["//remember current SP"
                  SP! "D=M" (at! "R15") "M=D"
                  (at! ret_addr)
                  "D=A" SP! "A=M" "M=D"
                  (incSP)
                  (at! (_SEGS_ sm-local)) "D=M"
                  SP! "A=M" "M=D"
                  (incSP)
                  (at! (_SEGS_ sm-args))
                  "D=M"
                  SP!
                  "A=M" "M=D"
                  (incSP)
                  (at! (_SEGS_ sm-this))
                  "D=M"
                  SP!
                  "A=M" "M=D"
                  (incSP)
                  (at! (_SEGS_ sm-that))
                  "D=M"
                  SP!
                  "A=M" "M=D"
                  (incSP)
                  (at! "R15") "D=M" ""])]
    (doseq [i (range args)] (s/sb+ s "D=D-1\n"))
    (str out
         s
         (csj [(at! (_SEGS_ sm-args))
               "// ARG -> old-SP - n-args"
               "M=D"
               ;; set LCL to be same as SP
               SP! "D=M"
               (at! (_SEGS_ sm-local))
               "M=D"
               (at! func)
               "0;JMP"
               (str "(" ret_addr ")")]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeASM "" [ctx ln buffer]
  (let [{:keys [action segment
                offset label func args]}
        @ln
        out
        (case action
          "push" (handlePush ctx segment offset)
          "pop" (handlePop ctx segment offset)
          "add" (handleAdd ctx)
          "sub" (handleSub ctx)
          "neg" (handleNeg ctx)
          "not" (handleNot ctx)
          "and" (handleAnd ctx)
          "or" (handleOr ctx)
          "eq" (handleEQ ctx)
          "gt" (handleGT ctx)
          "lt" (handleLT ctx)
          "if-goto" (handleIfGoto ctx label)
          "label" (handleLabelDef ctx label)
          "goto" (handleGoto ctx label)
          "function" (handleFunc ctx func args)
          "return" (handleFuncReturn ctx)
          "call" (handleCall ctx func args)
          (c/throwBadData "bad action: %s" action))]
    (s/sb+ buffer out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass2 "" [ctx total]
  (c/sreduce<>
    #(if (:blank? (deref %2)) %1 (writeASM ctx %2 %1)) total))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanFile "" [fp fout]
  (let [ctx (GenericMutable.
              {:fname (.getName ^File fp)
               :vmvars {}
               :staticOffset 0})]
    (c/prn!! "Processing file: %s" fp)
    (c/prn!! "Writing file: %s" fout)
    (->> (io/as-url fp) (pass1 ctx) (pass2 ctx) (spit fout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]

  (let [[src des & more] args]
    (if (and (s/hgl? src)
             (s/hgl? des)
             (empty? more))
      (try
        (scanFile (io/file src)
                  (io/file des))
        (catch Throwable e
          (.printStackTrace e)))
      (c/prn!! "Usage: vm <vm-file> <output-file>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

