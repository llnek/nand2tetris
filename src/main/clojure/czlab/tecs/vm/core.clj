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

  (:import [java.io File]
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

;private long _counter= 1L;
;private int _staticOffset= 0;
;private def _curFunc="",
;_curFile="",
;_VMVARS= [:];

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]
  (try
    (scanOneFile (first args))
    (catch Throwable t
      (.printStackTrace t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanOneFile "" [fp]
  (-> fp io/file io/as-url pass1 pass2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass2 "" [total] (c/sreduce<> #(writeASM %2 %1) total))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeASM "" [ln  buffer]
  (let [{:keys [action segment
                offset label func args]}
        @ln
        out
        (case action
          "push" (handlePush segment offset)
          "pop" (handlePop segment offset)
          "add" (handleAdd)
          "sub" (handleSub)
          "neg" (handleNeg)
          "not" (handleNot)
          "and" (handleAnd)
          "or" (handleOr)
          "eq" (handleEQ)
          "gt" (handleGT)
          "lt" (handleLT)
          "if-goto" (handleIfGoto label)
          "label" (handleLabelDef label)
          "goto" (handleGoto lable)
          "function" (handleFunc func args)
          "return" (handleFuncReturn)
          "call" (handleCall func args)
          (c/trap! Exception "bad action"))]
    (.append buffer out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass1 "" [^URL furl]
  (with-open [inp (-> (.openStream furl)
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
  private def handlePush(String segment, int offset) {
    def out= "", vn, seg= segment.toLowerCase(),
    deal_with_spec_segs = { b,i ->
      return "@" + i + "\nD=A\n@"+ b + "\nA=M+D\nD=M\n";
    };

    if ("static".equals(seg)) {
      vn = _curFile + "." + offset;
      if ( ! _VMVARS.containsKey(vn)) {
        _VMVARS.put(vn, _staticOffset);
        ++_staticOffset;
      }
      offset = _VMVARS.get(vn);
    }

    // get value from the [ segment+offset ],put it into D-Reg
    switch (seg) {

      case "constant":
        out= "@" + offset + " // constant here\nD=A\n";
      break;

      case "temp":
        out= "@" + ( _REG_TEMP + offset) + " // temp here\nD=M\n";
      break;

      case "static":
        out= "@" + (_STATICS + offset) + " // static here\nD=M\n";
      break;

      case "pointer":
        if (offset==0) { out = "@THIS"; } else { out= "@THAT"; }
        out= out + " // pointer here\nD=M\n";
      break;

      case "argument": out= deal_with_spec_segs("ARG", offset); break;
      case "local": out= deal_with_spec_segs("LCL", offset); break;
      case "this": out= deal_with_spec_segs("THIS", offset); break;
      case "that": out= deal_with_spec_segs("THAT", offset); break;

      default:
        throw new Exception("Unexpected segment: " + segment);
      break;
    }

    // we have the target data in D-REG
    // set value to current stack pointer [sptr] = D
    // then inc the stack pointer by 1
    out = out + pushSPWithD() + incSP();
    return out;
  }

  /**/
  private def setValInR13ToAddrInR14() {
    return "@R13\nD=M\n@R14\nA=M\nM=D\n";
  }

  /**/
  private def popSPWithR13() {
    return "@SP\nA=M-1\nD=M\n@R13\nM=D\n" ;
  }

  /**/
  private def setDRegAsAddrInR14(String lbl, int offset, String cmt) {

    def s="";
    if (lbl==null || lbl.length() ==0) {
      s="@" + offset + "  // " + cmt + "\n" +
      "D=A\n@R14\nM=D\n" ;
    }
    else {
      s = "@" + offset + "\n" +
      "D=A\n" + "@" + lbl + "\n" +
      "D=D+M\n@R14\nM=D\n";
    }
    return s;
  }

  /**/
  private def pushSPWithD() {
    return "@SP\nA=M\nM=D\n";
  }

  /**/
  private def incSP() {
    return "@SP\nM=M+1\n";
  }

  /**/
  private def decSP() {
    return "@SP\nM=M-1\n";
  }

  /**/
  private def handlePop(String segment, int offset) {
    def me=this, vn, out= "", seg= segment.toLowerCase(),
    deal_with_spec_segs = { b,i, c ->
      return me.setDRegAsAddrInR14(b, i, c);
    };


    if ("static".equals(seg)) {
      vn = _curFile + "." + offset;
      if ( ! _VMVARS.containsKey(vn)) {
        _VMVARS.put(vn, _staticOffset);
        ++_staticOffset;
      }
      offset = _VMVARS.get(vn);
    }

    // pop value off the stack and
    // set value to the [ segment+offset ]

    // we store the target RAM address int R14 first
    // once we get the value off the stack,
    // then set the value into the target address

    switch (seg) {

      case "constant":
        throw new Exception("Unexpected pop of constant segment!");
      break;

      case "pointer":
        if (offset == 0) { offset = _REG_THIS; } else { offset= _REG_THAT; }
        out= setDRegAsAddrInR14("", offset, "pointer");
      break;

      case "temp":
        out= setDRegAsAddrInR14("", _REG_TEMP + offset, "temp-offset");
      break;

      case "static":
        out= setDRegAsAddrInR14("", _STATICS + offset, "static-offset");
      break;

      case "argument": out=deal_with_spec_segs("ARG", offset, "arg-offset"); break;
      case "local": out=deal_with_spec_segs("LCL", offset, "lcl-offset"); break;
      case "this": out=deal_with_spec_segs("THIS", offset, "this-offset"); break;
      case "that": out=deal_with_spec_segs("THAT", offset, "this-that"); break;

      default:
        throw new Exception("Unexpected segment: " + segment);
      break;
    }

    out = popSPWithR13() + decSP() + out + setValInR13ToAddrInR14();
    return out;
  }

  /**/
  private def handleAdd() {
    return "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D+M\n" + decSP();
  }

  /**/
  private def handleSub() {
    return "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=M-D\n" + decSP();
  }

  /**/
  private def handleAnd() {
    return "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D&M\n" + decSP();
  }

  /**/
  private def handleOr() {
    return "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nM=D|M\n" + decSP();
  }

  /**/
  private def handle_bool_0() {
    // D has the value of x-y
    return "@SP\nA=M-1\nD=M\n@SP\nA=M-1\nA=A-1\nD=M-D\n";
  }
  /**/
  private def handle_bool_1(cp,t,f) {
    return "@" + t + "\nD;" + cp + "\n@" + f + "\n0;JMP\n";
  }
  /**/
  private def handle_bool_toggles(t,f,z) {
    return "(" + t + ")\n@SP\nA=M-1\nA=A-1\nM=-1\n@" + z + "\n0;JMP\n" +
    "(" + f + ")\n@SP\nA=M-1\nA=A-1\nM=0\n@" + z + "\n0;JMP\n"+
    "(" + z+ ")\n" + decSP();
  }

  /**/
  private def handleEQ() {
    def lbl= nextAutoLabel(""), t= lbl+".t", f= lbl+".f", z= lbl+".z";
    return handle_bool_0() + handle_bool_1("JEQ",t,f) + handle_bool_toggles(t,f,z);
  }

  /**/
  private def handleGT() {
    def lbl= nextAutoLabel(""), t= lbl+".t", f= lbl+".f", z= lbl+".z";
    return handle_bool_0() + handle_bool_1("JGT",t,f) + handle_bool_toggles(t,f,z);
  }

  /**/
  private def handleLT() {
    def lbl= nextAutoLabel(""), t= lbl+".t", f= lbl+".f", z= lbl+".z";
    return handle_bool_0() + handle_bool_1("JLT",t,f) + handle_bool_toggles(t,f,z);
  }

  /**/
  private def handleNeg() {
    return "@SP\nA=M-1\nD=-M\nM=D\n";
  }

  /**/
  private def handleNot() {
    return "@SP\nA=M-1\nD=!M\nM=D\n";
  }

  /**/
  private def handleLabelDef(lbl) {
    return "(" + lbl + ")\n";
  }

  /**/
  private def handleIfGoto(lbl) {
    return popSPWithR13() + decSP() + "@R13\nD=M\n@"+lbl+"\nD;JNE\n" ;
  }

  /**/
  private def handleGoto(lbl) {
    return "@" + lbl + "\n" + "0;JMP\n";
  }

  /**/
  private def handleFunc(fn, args) {
    def i, s,out= "(" + fn + ") // function-def starts here\n";

    _curFunc= fn;

    // deal with local vars , set them to zero, up the SP too, yes ?
    s="";
    for ( i=0; i < args; ++i) {
      if (i==0) { s += "// prepare local vars\n@LCL\nD=M\n" ; }
      else {
        s += "D=D+1\n";
      }
      s += "A=D\nM=0\n";
    }
    out = out + s;
    s="";
    for ( i=0; i < args; ++i) {
      if (i==0) {
        s += "// adjust SP to cater for added local vars\n@SP\nD=M\n" ;
      }
      s += "D=D+1\n";
    }
    if (s.length() > 0) {
        s += "@SP\nM=D\n" ;
    }

    out = out + s + "// starts code\n";
    return out;
  }

  /**/
  private def handleFuncReturn() {

    return    "// start the return nightmare....\n" +
    "@LCL\nD=M\n@R13\n" +
    "M=D // [13] => LCL -> frame\n" +

    "@R13\nD=M-1\nD=D-1\nD=D-1\nD=D-1\nA=D-1\nD=M\n"+
    "@R14\nM=D  // [14] -> return addr\n" +

    // move the SP back to old place, also place the return value there
    "@ARG\nD=M\n@R15\nM=D\n@SP\nA=M-1\nD=M\n@R15\nA=M\nM=D\n" +
    "@R15\nD=M\nD=D+1\n@SP\nM=D\n" +

    "@R13\nA=M-1\nD=M\n@THAT\n" +
    "M=D // reset THAT\n" +

    "@R13\nD=M-1\nA=D-1\nD=M\n@THIS\n" +
    "M=D  // reset THIS\n" +

    "@R13\nD=M-1\nD=D-1\nA=D-1\nD=M\n@ARG\n" +
    "M=D // reset ARG\n" +

    "@R13\nD=M-1\nD=D-1\nD=D-1\nA=D-1\nD=M\n@LCL\n" +
    "M=D  // reset LCL\n"+

    "@R14\nA=M\n" +
    "0;JMP      // return code\n";
  }

  /**/
  private def handleCall(fn, args) {
    def ret_addr= nextAutoLabel("fc"),
    s,
    out= "// remember current SP\n" +
    "@SP\nD=M\n@R15\nM=D\n"+

    "@" + ret_addr + " // push ret-addr to stack\n" +
    "D=A\n@SP\nA=M\nM=D\n" +
    incSP() +

    "@LCL // push old LCL to stack\n"+
    "D=M\n@SP\nA=M\nM=D\n" +
    incSP() +

    "@ARG // push old ARG to stack\n" +
    "D=M\n@SP\nA=M\nM=D\n" +
    incSP() +

    "@THIS // push old THIS to stack\n"+
    "D=M\n@SP\nA=M\nM=D\n" +
    incSP() +

    "@THAT // push old THAT to stack\n" +
    "D=M\n@SP\nA=M\nM=D\n" +
    incSP() +

    "@R15 // repos ARG for function\n" +
    "D=M\n" ;
    s="";
    for (int i=0; i < args; ++i) {
      s += "D=D-1\n";
    }
    out = out + s + "@ARG\n // ARG -> old-SP - n-args \n" +
    "M=D\n" +

    // set LCL to be same as SP
    "@SP\nD=M\n@LCL  // lcl -> SP\n" +
    "M=D\n" +

    "@" + fn + " // now jump to funct\n" +
    "0;JMP\n" +
    "(" + ret_addr + ")\n";

    return out;
  }

  /**/
  private def nextAutoLabel(String pf) {
    try {
      return pf + "." + System.currentTimeMillis().toString() + "." + _counter.toString();
    }
    finally {
      ++_counter;
    }
  }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


