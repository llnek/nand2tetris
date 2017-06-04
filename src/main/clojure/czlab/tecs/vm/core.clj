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

  (:import [java.util.concurrent.atomic AtomicInteger]
           [czlab.basal.core GenericMutable]
           [java.io File LineNumberReader]
           [java.net URL]
           [java.util Map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^String sm-argument "argument")
(def ^:private ^String sm-local "local")
(def ^:private ^String sm-static "static")
(def ^:private ^String sm-constant "constant")
(def ^:private ^String sm-this "this")
(def ^:private ^String sm-that "that")
(def ^:private ^String sm-pointer "pointer")
(def ^:private ^String sm-temp "temp")

(def ^:private ^String SM-CONSTANT (s/ucase sm-constant))
(def ^:private ^String SM-POINTER (s/ucase sm-pointer))
(def ^:private ^String SM-ARGUMENT "ARG")
(def ^:private ^String SM-LOCAL "LCL")
(def ^:private ^String SM-STATIC (s/ucase sm-static))
(def ^:private ^String SM-THIS (s/ucase sm-this))
(def ^:private ^String SM-THAT (s/ucase sm-that))
(def ^:private ^String SM-TEMP (s/ucase sm-temp))

(def ^:private _SP 256) ;; -> 2047
(def ^:private _CONST -999)
(def ^:private _PTR -888)
(def ^:private _REGS 0)    ;; -> 15
(def ^:private _STATICS 16)    ;; -> 255
(def ^:private _HEAP 2048) ;; -> 16383
(def ^:private _IO 26384)  ;; -> 24575
(def ^:private _REG_SP 0)
(def ^:private _REG_LCL 1)
(def ^:private _REG_ARG 2)
(def ^:private _REG_THIS 3)
(def ^:private _REG_THAT 4)
(def ^:private _REG_TEMP 5)    ;; -> 12
(def ^:private _REG_REG 13)    ;; -> 15

(def ^:private _counter (AtomicInteger.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pushSPWithD "" [] (cs/join "\n" ["@SP" "A=M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- incSP "" [] (cs/join "\n" ["@SP" "M=M+1" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- decSP "" [] (cs/join "\n" ["@SP" "M=M-1" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextAutoLabel "" [pf]
  (str pf "." (c/now<>) "." (.getAndIncrement ^AtomicInteger _counter)))

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
(defn thisthat<n> "" [offset] (if (= 0 offset) _REG_THIS _REG_THAT))
(defn thisthat<s> "" [offset] (if (= 0 offset) SM-THIS SM-THAT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handlePush "" [ctx segment offset]
  (let [spec-segs #(str "@" %2
                        "\nD=A\n"
                        "@" %1
                        "\nA=M+D\nD=M\n")
        seg (s/lcase segment)
        offset
        (if (= "static" seg)
          (let [{:keys [staticOffset
                        fname vmvars]} @ctx
                vn (str fname "." offset)]
            (if-not (c/in? vmvars vn)
              (c/copy* ctx {:vmvars (assoc vmvars vn staticOffset)
                            :staticOffset (inc staticOffset)}))
            (get (:vmvars @ctx) vn))
          offset)
        out (s/strbf<>)]
    ;; get value from [segment+offset],put it into D-Reg
    (cond
      (= seg sm-constant)
      (s/sb+ out "@" offset "\nD=A\n")
      (= seg sm-temp)
      (s/sb+ out "@" (+ _REG_TEMP offset) "\nD=M\n")
      (= seg sm-static)
      (s/sb+ out "@" (+ _STATICS offset) "\nD=M\n")
      (= seg sm-pointer)
      (s/sb+ out "@" (thisthat<s> offset) "\nD=M\n")
      (= seg sm-argument)
      (s/sb+ out (spec-segs SM-ARGUMENT offset))
      (= seg sm-local)
      (s/sb+ out (spec-segs SM-LOCAL offset))
      (= seg sm-this)
      (s/sb+ out (spec-segs SM-THIS offset))
      (= seg sm-that)
      (s/sb+ out (spec-segs SM-THAT offset))
      :else
      (c/trap! Exception (format "Unexpected segment: %s" segment)))
    ;; we have the target data in D-REG
    ;; set value to current stack pointer [sptr] = D
    ;; then inc the stack pointer by 1
    (str (s/sb+ out (pushSPWithD) (incSP)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setValInR13ToAddrInR14 "" []
  (cs/join "\n" ["@R13" "D=M" "@R14" "A=M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popSPWithR13 "" []
  (cs/join "\n" ["@SP" "A=M-1" "D=M" "@R13" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setDRegAsAddrInR14 "" [lbl offset cmt]
  (->>
    (if (s/nichts? lbl)
      [(str "@" offset) "D=A" "@R14" "M=D" ""]
      [(str "@" offset) "D=A" (str "@" lbl) "D=D+M" "@R14" "M=D" ""])
    (cs/join "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handlePop "" [ctx segment offset]
  (let [seg (s/lcase segment)
        out (s/strbf<>)
        offset
        (if (= sm-static seg)
          (let [{:keys [fname vmvars
                        staticOffset]} @ctx
                vn (str fname "." offset)]
            (if-not (c/in? vmvars vn)
              (c/copy* ctx {:vmvars (assoc vmvars vn staticOffset)
                            :staticOffset (inc staticOffset)}))
            (get (:vmvars @ctx) vn))
          offset)]
    ;; pop value off the stack and
    ;; set value to the [ segment+offset ]
    ;; we store the target RAM address int R14 first
    ;; once we get the value off the stack,
    ;; then set the value into the target address
    (cond
      (= seg sm-constant)
      (c/trap! Exception "Cannot pop constant segment!")
      (= seg sm-pointer)
      (s/sb+ out
             (setDRegAsAddrInR14 ""
                                 (thisthat<n> offset) "pointer"))
      (= seg sm-temp)
      (s/sb+ out (setDRegAsAddrInR14 "" (+ _REG_TEMP offset) "temp-offset"))
      (= seg sm-static)
      (s/sb+ out (setDRegAsAddrInR14 "" (+ _STATICS offset) "static-offset"))
      (= seg sm-argument)
      (s/sb+ out (setDRegAsAddrInR14 SM-ARGUMENT offset "arg-offset"))
      (= seg sm-local)
      (s/sb+ out (setDRegAsAddrInR14 SM-LOCAL offset "lcl-offset"))
      (= seg sm-this)
      (s/sb+ out (setDRegAsAddrInR14 SM-THIS offset "this-offset"))
      (= seg sm-that)
      (s/sb+ out (setDRegAsAddrInR14 SM-THAT offset "this-that"))
      :else
      (c/trap! Exception (format "Unexpected segment: %s" segment)))
    (str (popSPWithR13)
         (decSP)
         out
         (setValInR13ToAddrInR14))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAdd "" [ctx]
  (cs/join "\n"
           ["@SP" "A=M-1" "D=M" "@SP" "A=M-1" "A=A-1" "M=D+M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleSub "" [ctx]
  (cs/join "\n"
           ["@SP" "A=M-1" "D=M" "@SP" "A=M-1" "A=A-1" "M=M-D" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAnd "" [ctx]
  (cs/join "\n"
           ["@SP" "A=M-1" "D=M" "@SP" "A=M-1" "A=A-1" "M=D&M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleOr "" [ctx]
  (cs/join "\n"
           ["@SP" "A=M-1" "D=M" "@SP" "A=M-1" "A=A-1" "M=D|M" (decSP)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_0 "" []
  ;; D has the value of x-y
  (cs/join "\n" ["@SP" "A=M-1" "D=M" "@SP" "A=M-1" "A=A-1" "D=M-D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_1 "" [cp t f]
  (cs/join "\n"
           [(str "@" t)
            (str "D;" cp) (str "@" f) (str "0;JMP") ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_toggles "" [t f z]
  (cs/join "\n"
           [(str "(" t ")")
            "@SP"
            "A=M-1"
            "A=A-1"
            "M=-1"
            (str "@" z)
            "0;JMP"
            (str "(" f ")")
            "@SP"
            "A=M-1"
            "A=A-1"
            "M=0"
            (str "@" z)
            "0;JMP"
            (str "(" z ")") (decSP)]))

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
(defn- handleNeg "" [ctx] (cs/join "\n"
                                   ["@SP" "A=M-1" "D=-M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleNot "" [ctx] (cs/join "\n" ["@SP" "A=M-1" "D=!M" "M=D" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleLabelDef "" [ctx lbl] (str "(" lbl ")\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleIfGoto "" [ctx lbl]
  (str (popSPWithR13)
       (decSP)
       (cs/join "\n" ["@R13" "D=M" (str "@" lbl) "D;JNE" ""])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleGoto "" [ctx lbl]
  (cs/join "\n" [(str "@" lbl) "0;JMP" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFunc "" [ctx func args]
  (let [out (s/strbf<> (str "(" func ")\n"))
        s (s/strbf<>)]
    (c/setf! ctx :func func)
    ;; deal with local vars ,
    ;; set them to zero, up the SP too, yes ?
    (doseq [i (range args)]
      (if (= i 0)
        (s/sb+ s (cs/join "\n" ["" "@LCL" "D=M" ""]))
        (s/sb+ s "D=D+1\n"))
      (s/sb+ s (cs/join "\n" ["A=D" "M=0" ""])))
    (s/sb+ out s)
    (.setLength s 0)
    ;; adjust SP to cater for added local vars
    (doseq [i (range args)]
      (if (= 0 i)
        (s/sb+ s (cs/join "\n" ["" "@SP" "D=M" ""])))
      (s/sb+ s "D=D+1\n"))
    (if (pos? (.length s))
      (s/sb+ s (cs/join "\n" ["@SP" "M=D" ""])))
    (str (s/sb+ out s "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFuncReturn "" [ctx]
  (cs/join "\n"
           ["//start the return nightmare...."
            "@LCL" "D=M" "@R13"
            "M=D // [13] => LCL -> frame"
            "@R13" "D=M-1"
            "D=D-1" "D=D-1" "D=D-1" "A=D-1" "D=M"
            "@R14" "M=D // [14] -> return addr"
            ;; move the SP back to old place, also place the return value there
            "@ARG" "D=M" "@R15" "M=D"
            "@SP" "A=M-1" "D=M" "@R15" "A=M" "M=D"
            "@R15" "D=M" "D=D+1" "@SP" "M=D"
            "@R13" "A=M-1" "D=M" "@THAT"
            "M=D // reset THAT"
            "@R13" "D=M-1" "A=D-1" "D=M" "@THIS"
            "M=D  // reset THIS"
            "@R13" "D=M-1" "D=D-1" "A=D-1" "D=M" "@ARG"
            "M=D // reset ARG"
            "@R13" "D=M-1" "D=D-1" "D=D-1" "A=D-1" "D=M" "@LCL"
            "M=D // reset LCL"
            "@R14" "A=M"
            "0;JMP // return code"
            ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleCall "" [ctx func args]
  (let [ret_addr (nextAutoLabel "fc")
        s (s/strbf<>)
        out
        (s/strbf<>
          (cs/join "\n"
                   ["// remember current SP"
                    "@SP" "D=M" "@R15" "M=D"
                    (str "@" ret_addr)
                    "D=A" "@SP" "A=M" "M=D"
                    (incSP)
                    "@LCL"
                    "D=M" "@SP" "A=M" "M=D"
                    (incSP)
                    "@ARG"
                    "D=M" "@SP" "A=M" "M=D"
                    (incSP)
                    "@THIS"
                    "D=M" "@SP" "A=M" "M=D"
                    (incSP)
                    "@THAT"
                    "D=M" "@SP" "A=M" "M=D"
                    (incSP)
                    "@R15" "D=M" ""]))]
    (doseq [i (range args)] (s/sb+ s "D=D-1\n"))
    (str
      (s/sb+ out
             s
             (cs/join "\n"
                      ["@ARG"
                       "// ARG -> old-SP - n-args"
                       "M=D"
                       ;; set LCL to be same as SP
                       "@SP" "D=M"
                       "@LCL // lcl -> SP"
                       "M=D"
                       (str "@" func)
                       "0;JMP"
                       (str "(" ret_addr ")")])))))

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
          (c/trap! Exception (format "bad action: %s" action)))]
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
              {:fname (.getName ^File fp)})]
    (c/prn!! "Processing file: %s" fp)
    (c/prn!! "Writing file: %s" fout)
    (->> (io/as-url fp) (pass1 ctx) (pass2 ctx) (spit fout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]

  (let [[src des & more] args]
    (if (and (some? src)
             (some? des)
             (empty? more))
      (try
        (scanFile (io/file src)
                  (io/file des))
        (catch Throwable e
          (.printStackTrace e)))
      (c/prn!! "Usage: vm <vm-file> <output-file>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


