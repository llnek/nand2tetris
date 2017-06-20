;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.cmp.cgen

  (:require [czlab.basal.format :as f]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:use [clojure.walk])

  (:import [java.io Writer File]
           [java.util.concurrent.atomic AtomicInteger]
           [czlab.tecs.p11 Node ASTNode ASTGentor]
           [czlab.basal.core GenericMutable]
           [java.net URL]
           [java.util Stack ArrayList List Map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:dynamic *class-level?* false)
(def ^:dynamic *class-ctor?* false)
(def ^:dynamic *static-syms* nil)
(def ^:dynamic *class-name* "")
(def ^:dynamic *func-info* nil)
(def ^:dynamic *class-syms* nil)

(def ^:dynamic *local-syms* nil)
(def ^:dynamic *arg-syms* nil)

(def ^AtomicInteger while-cntr (AtomicInteger.))
(def ^AtomicInteger if-cntr (AtomicInteger.))
(def ^AtomicInteger static-cntr (AtomicInteger.))
(def ^AtomicInteger field-cntr (AtomicInteger.))
(def ^AtomicInteger arg-cntr (AtomicInteger.))
(def ^AtomicInteger var-cntr (AtomicInteger.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private nline "" [w] `(.write ~w "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare genExpr genStmts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- err-undef-var "" ^String [varr]
  (format "In subroutine %s: %s is not defined", (:name *func-info*) varr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isPrimType? "" [t]
  (or (= "boolean" t)
      (= "int" t)
      (= "char" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genAction "" [^Writer w op & [unary?]]
  (when-some
    [s (condp = op
         "-" (if unary? "neg" "sub")
         "return" "return"
         "+" "add"
         "*" "call Math.multiply 2"
         "/" "call Math.divide 2"
         "~" "not"
         ">" "gt"
         "<" "lt"
         "=" "eq"
         "&" "and"
         "|" "or"
         nil)]
    (.write w s)
    (nline w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genPush "" [^Writer w what & more]
  (->> (s/strim (cs/join " " more))
       (str "push " what " ")
       (.write w))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genCall "" [^Writer w func numArgs]
  (.write w (str "call " func " " numArgs))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genPop "" [^Writer w what & more]
  (->> (s/strim (cs/join " " more))
       (str "pop " what " ")
       (.write w))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- findSymbol "" [varr must?]
  (let [info (if *class-level?*
               (or (get *local-syms* varr)
                   (get *arg-syms* varr)
                   (get *class-syms* varr)
                   (get *static-syms* varr))
               (or (get *local-syms* varr)
                   (get *arg-syms* varr)
                   (get *static-syms* varr)))]
    (if (and (nil? info) must?)
      (c/trap! Exception (err-undef-var varr)) info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genIfGoto "" [^Writer w lbl]
  (.write w (str "if-goto " lbl))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genGoto "" [^Writer w lbl]
  (.write w (str "goto " lbl))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genLabel "" [^Writer w lbl]
  (.write w (str "label " lbl))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getMemLoc "" [^Writer w loc index]
  (genPush w
           (if (= "field" loc) "this" loc) index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setMemLoc "" [^Writer w loc index]
  (genPop w
          (if (= "field" loc) "this" loc) index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genString "" [^Writer w ^String s]
  (let [len (.length s)]
    (genPush w "constant" len)
    (genCall w "String.new" 1)
    ;;appendChar returns this
    (doseq [c s]
      (genPush w "constant" (int c))
      (genCall w "String.appendChar" 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genKeyword "" [^Writer w kw]
  (case kw
    ("null" "false")
    (genPush w "constant" 0)
    "true"
    (do (genPush w "constant" 0)
        (genAction w "~"))
    "this"
    (genPush w "pointer" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genLiteral "" [^Writer w
                      {:keys [literal value] :as term}]
  (condp = literal
    "String"
    (genString w value)
    "int"
    (genPush w "constant" value)
    "keyword"
    (genKeyword w value)
    (c/trap! Exception
             (str "bad literal: " literal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genVarr "" [^Writer w varr idx]
  (let [info (findSymbol varr true)
        {:keys [mtype index]} info]
    (if (nil? idx)
      (getMemLoc w mtype index)
      (do
        (genExpr w idx)
        (getMemLoc w mtype index)
        (genAction w "+")
        (genPop w "pointer" 1)
        (genPush w "that" 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genInvoke "" [^Writer w call]
  (let [{:keys [target params]}
        call
        dt (atom nil)
        [z m] (.split ^String
                      target "\\.")
        cnt (count params)]
    (cond
      (= "this" z)
      (genPush w "this" 0)
      (s/hgl? z)
      (let [info (findSymbol z false)
            {:keys [mtype
                    index dtype]}
            info]
        (when (some? info)
          (reset! dt dtype)
          (getMemLoc w mtype index))))
    (doseq [p params]
      (genExpr w p))
    (->>
      (if (or (= "this" z)
              (and (some? @dt)
                   (not (isPrimType? dt))))
        (inc cnt)
        cnt)
      (genCall w
               (str (if (some? @dt) @dt z) "." m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genTerm "" [^Writer w
                   {:keys [literal varr index
                           call group]
                    :as tm}]
  (cond
    (some? literal)
    (genLiteral w tm)
    (some? call)
    (genInvoke w call)
    (some? group)
    (genExpr w group)
    (some? varr)
    (genVarr w varr index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genExpr "" [^Writer w expr]
  (doseq [tm (:children expr)
          :let [op? (= :OP (:tag tm))]]
    (if op?
      (genAction w (:value tm) (:unary tm))
      (genTerm w tm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genLet "" [^Writer w
                  {:keys [varr lhs rhs] :as stmt}]
  (let [info (findSymbol varr true)
        {:keys [mtype index]} info]
    ;;eval and set the right offset to the object-array
    (when (some? lhs)
      (genExpr w lhs)
      (getMemLoc w mtype index)
      (genAction w "+"))
    ;;eval the statement
    (genExpr w rhs)
    ;;save the rhs result
    (genPop w "temp" 0)
    ;;do the assignment
    (if (some? lhs)
      (do
        (genPop w "pointer" 1)
        (genPush w "temp" 0)
        (genPop w "that" 0))
      (do
        (genPush w "temp" 0)
        (setMemLoc w mtype index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genIf "" [^Writer w
                 {:keys [test then else] :as stmt}]
  (let [cnt (.getAndIncrement if-cntr)
        ttag (str "IF_TRUE_" cnt)
        etag (str "IF_ELSE_" cnt)
        dtag (str "IF_DONE_" cnt)]
    (genExpr w test)
    (genIfGoto w ttag)
    (genGoto w etag)
    (genLabel w ttag)
    (genStmts w then)
    (genGoto w dtag)
    (genLabel w etag)
    (if (some? else)
      (genStmts w else))
    (genLabel w dtag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genWhile "" [^Writer w
                   {:keys [test body] :as stmt}]
  (let [cnt (.getAndIncrement while-cntr)
        stag (str "WHILE_LOOP_" cnt)
        etag (str "WHILE_END_" cnt)]
    (genLabel w stag)
    (genExpr w test)
    (genAction w "~")
    (genIfGoto w etag)
    (genStmts w body)
    (genGoto w stag)
    (genLabel w  etag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genDo "" [^Writer w
                {:keys [call] :as stmt}]
  (genInvoke w call)
  (genPop w "temp" 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genReturn "" [^Writer w
                    {:keys [value] :as stmt}]
  (if (some? value)
    (genExpr w value)
    (genPush w "constant" 0))
  (genAction w "return"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genStmts "" [^Writer w stmts]
  (doseq [s stmts
          :let [t (:tag s)]]
    (condp = t
      :WhileStatement
      (genWhile w s)
      :LetStatement
      (genLet w s)
      :IfStatement
      (genIf w s)
      :DoStatement
      (genDo w s)
      :ReturnStatement
      (genReturn w s)
      (c/trap! Exception (str "bad stmt: " t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- genCtor "" [^Writer w]
  (let [cnt (count *class-syms*)]
    (genPush w "constant" cnt)
    (genCall w "Memory.alloc" 1)
    (genPop w "pointer" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setThisPtr "" [^Writer w]
  (getMemLoc w "argument" 0)
  (genPop w "pointer" 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genFnBody "" [^Writer w stmts]
  (cond
    *class-ctor?* (genCtor w)
    *class-level?* (setThisPtr w))
  (genStmts w stmts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genFnHeader ""
  [^Writer w {:keys [arg-count
                     isMethod?
                     type
                     name
                     var-count] :as gist}]
  (.write w
          (str "function "
               *class-name* "." name " " var-count))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



