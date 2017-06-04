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

  (:require [czlab.basal.log :as log]
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

(def ^:private _SEGREGS {sm-argument _REG_ARG
                         sm-local _REG_LCL
                         sm-static _STATICS
                         sm-this _REG_THIS
                         sm-that  _REG_THAT
                         sm-temp _REG_TEMP
                         sm-constant _CONST
                         sm-pointer _PTR})

(def ^:private _counter (AtomicInteger.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nextAutoLabel "" [pf]
  (str pf
       "."
       (System/currentTimeMillis)
       "."
       (.getAndIncrement _counter)))

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
          "goto" (handleGoto ctx lable)
          "function" (handleFunc ctx func args)
          "return" (handleFuncReturn ctx)
          "call" (handleCall ctx func args)
          (c/trap! Exception "bad action"))]
    (.append buffer out)))

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
(defn- handlePush "" [ctx segment offset]
  (let [seg (s/lcase segment)
        deal_with_spec_segs #(str "@" %2 "\nD=A\n@" %1 "\nA=M+D\nD=M\n")
        offset
        (if (= "static" segment)
          (let [{:keys [staticOffset fname vmvars]} @ctx
                vn (str fname "." offset)]
            (when-not (c/in? vmvars vn)
              (c/copy* ctx {:vmvars (assoc vmvars vn staticOffset)
                            :staticOffset (inc staticOffset)}))
            (get (:vmvars @ctx) vn))
          offset)
        ;; get value from the [segment+offset],put it into D-Reg
        out (s/strbf<>)]
    (case segment
      "constant"
      (.append out (str "@" offset " //constant here\nD=A\n"))
      "temp"
      (.append out (str "@" (+ _REG_TEMP offset) " // temp here\nD=M\n"))
      "static"
      (.append out (str "@" (+ _STATICS offset) " // static here\nD=M\n"))
      "pointer"
      (.append out (str (if (= 0 offset) "@THIS" "@THAT")
                        " // pointer here\nD=M\n"))
      "argument"
      (.append out (deal_with_spec_segs "ARG" offset))
      "local"
      (.append out (deal_with_spec_segs "LCL" offset))
      "this"
      (.append out (deal_with_spec_segs "THIS" offset))
      "that"
      (.append out (deal_with_spec_segs "THAT" offset))
      (c/trap! Exception (format "Unexpected segment: %s" segment)))
    ;; we have the target data in D-REG
    ;; set value to current stack pointer [sptr] = D
    ;; then inc the stack pointer by 1
    (.append out (str (pushSPWithD) (incSP)))
    (str out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setValInR13ToAddrInR14 "" [] "@R13\nD=M\n@R14\nA=M\nM=D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- popSPWithR13 "" [] "@SP\nA=M-1\nD=M\n@R13\nM=D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setDRegAsAddrInR14 "" [lbl offset cmt]
  (if (s/nichts? lbl)
    (str "@" offset "  // " cmt "\n" "D=A\n@R14\nM=D\n")
    (str "@" offset "\n" "D=A\n" "@" lbl "\n" "D=D+M\n@R14\nM=D\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pushSPWithD "" [] "@SP\nA=M\nM=D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- incSP "" [] "@SP\nM=M+1\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- decSP "" [] "@SP\nM=M-1\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handlePop "" [ctx segment offset]
  (let [seg (s/lcase seqment)
        out (s/strbf<>)
        offset
        (if (= "static" seg)
          (let [{:keys [fname vmvars statisOffset]} @ctx
                vn (str fname "." offset)]
            (when-not (c/in? vmvars vn)
              (c/copy* ctx {:vmvars (assoc vmvars vn staticOffset)
                            :staticOffset (inc staticOffset)}))
            (get (:vmvars @ctx) vn))
          offset)]
    ;; pop value off the stack and
    ;; set value to the [ segment+offset ]
    ;; we store the target RAM address int R14 first
    ;; once we get the value off the stack,
    ;; then set the value into the target address
    (case seg
      "constant"
      (c/trap! Exception "Cannot pop constant segment!")
      "pointer"
      (.append out
               (setDRegAsAddrInR14 ""
                                   (if (= offset 0)
                                     _REG_THIS _REG_THAT)
                                   "pointer"))
      "temp"
      (.append out (setDRegAsAddrInR14 "" (+ _REG_TEMP offset) "temp-offset"))
      "static"
      (.append out (setDRegAsAddrInR14 "" (+ _STATICS offset) "static-offset"))
      "argument"
      (.append out (setDRegAsAddrInR14 "ARG" offset "arg-offset"))
      "local"
      (.append out (setDRegAsAddrInR14 "LCL" offset "lcl-offset"))
      "this"
      (.append out (setDRegAsAddrInR14 "THIS" offset "this-offset"))
      "that"
      (.append out (setDRegAsAddrInR14 "THAT" offset "this-that"))
      (c/trap! Exception (format "Unexpected segment: %s" segment)))

    (str (popSPWithR13) (decSP) out (setValInR13ToAddrInR14))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAdd "" [ctx]
    (str "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D+M\n" (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleSub "" [ctx]
  (str "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=M-D\n" (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleAnd "" [ctx]
  (str "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D&M\n" (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleOr "" []
  (str "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D|M\n" (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_0 "" []
  ;; D has the value of x-y
  "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nD=M-D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_1 "" [cp t f]
  (str "@" t "\nD;" cp "\n@" f "\n0;JMP\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handle_bool_toggles "" [t f z]
  (str "(" t ")\n@SP\nA=M-1\nA=A-1\nM=-1\n@" z "\n0;JMP\n"
       "(" f ")\n@SP\nA=M-1\nA=A-1\nM=0\n@" z "\n0;JMP\n"
       "(" z+ ")\n" (decSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleEQ "" []
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JEQ" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleGT "" []
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JGT" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleLT "" []
  (let [lbl (nextAutoLabel "")
        t (str lbl ".t")
        f (str lbl ".f")
        z (str lbl ".z")]
    (str (handle_bool_0)
         (handle_bool_1 "JLT" t f)
         (handle_bool_toggles t f z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleNeg "" [] "@SP\nA=M-1\nD=-M\nM=D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleNot "" [] "@SP\nA=M-1\nD=!M\nM=D\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleLabelDef "" [lbl] (str "(" lbl ")\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleIfGoto "" [lbl]
  (str (popSPWithR13) (decSP) "@R13\nD=M\n@" lbl "\nD;JNE\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleGoto "" [lbl] (str "@" lbl "\n" "0;JMP\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFunc "" [ctx func args]
  (let [out (s/strbf<>
              (str "("
                   func
                   ") // function-def starts here\n"))
        s (s/strbf<>)]
    (c/setf! ctx :func func)
    ;; deal with local vars , set them to zero, up the SP too, yes ?
    (doseq [i (range args)]
      (if (= i 0)
        (.append s "// prepare local vars\n@LCL\nD=M\n")
        (.append s "D=D+1\n"))
      (.append s "A=D\nM=0\n"))
    (.append out s)
    (.setLength s 0)
    (doseq [i (range args)]
      (if (= 0 i)
        (.append s "// adjust SP to cater for added local vars\n@SP\nD=M\n"))
      (.append s "D=D+1\n"))
    (if (pos? (.length s))
      (.append s "@SP\nM=D\n"))
    (.append out (str s "// starts code\n"))
    (str out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFuncReturn "" [ctx]
  (str "// start the return nightmare....\n"
       "@LCL\nD=M\n@R13\n"
       "M=D // [13] => LCL -> frame\n"

       "@R13\nD=M-1\nD=D-1\nD=D-1\nD=D-1\nA=D-1\nD=M\n"
       "@R14\nM=D  // [14] -> return addr\n"

       ;; move the SP back to old place, also place the return value there
       "@ARG\nD=M\n@R15\nM=D\n@SP\nA=M-1\nD=M\n@R15\nA=M\nM=D\n"
       "@R15\nD=M\nD=D+1\n@SP\nM=D\n"

       "@R13\nA=M-1\nD=M\n@THAT\n"
       "M=D // reset THAT\n"

       "@R13\nD=M-1\nA=D-1\nD=M\n@THIS\n"
       "M=D  // reset THIS\n"

       "@R13\nD=M-1\nD=D-1\nA=D-1\nD=M\n@ARG\n"
       "M=D // reset ARG\n"

       "@R13\nD=M-1\nD=D-1\nD=D-1\nA=D-1\nD=M\n@LCL\n"
       "M=D  // reset LCL\n"

       "@R14\nA=M\n"
       "0;JMP      // return code\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleCall "" [ctx func args]
  (let [ret_addr (nextAutoLabel "fc")
        s (s/strbf<>)
        out
        (s/strbf<>
          (str "// remember current SP\n"
               "@SP\nD=M\n@R15\nM=D\n"
               "@"  ret_addr  " // push ret-addr to stack\n"
               "D=A\n@SP\nA=M\nM=D\n"
               (incSP)
               "@LCL // push old LCL to stack\n"
               "D=M\n@SP\nA=M\nM=D\n"
               (incSP)
               "@ARG // push old ARG to stack\n"
               "D=M\n@SP\nA=M\nM=D\n"
               (incSP)
               "@THIS // push old THIS to stack\n"
               "D=M\n@SP\nA=M\nM=D\n"
               (incSP)
               "@THAT // push old THAT to stack\n"
               "D=M\n@SP\nA=M\nM=D\n"
               (incSP)
               "@R15 // repos ARG for function\n"
               "D=M\n"))]
    (doseq [i (range args)] (.append s "D=D-1\n"))
    (.append out
             (str s
                  "@ARG\n // ARG -> old-SP - n-args \n"
                  "M=D\n"
                  ;; set LCL to be same as SP
                  "@SP\nD=M\n@LCL  // lcl -> SP\n"
                  "M=D\n"
                  "@" func " // now jump to funct\n"
                  "0;JMP\n"
                  "(" ret_addr ")\n"))
    (str out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass2 "" [ctx total] (c/sreduce<> #(writeASM ctx %2 %1) total))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanOneFile "" [fp]
  (let [ctx (GenericMutable. {})
        f (io/file fp)]
    (c/setf! ctx :fname (.getName f))
    (->> (io/as-url f)
         (pass1 ctx)
         (pass2 ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]
  (try
    (scanOneFile (first args))
    (catch Throwable t
      (.printStackTrace t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


