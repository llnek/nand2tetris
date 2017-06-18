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
(def ^:dynamic *class-syms* nil)
(def ^:dynamic *funct-syms* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private genHeader "" [w fname info]
  `(.write ~w (str ~fname " " (count (:args ~info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private nline "" [w] `(.write ~w "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare genCall genPush genPop genAction genExpr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genString "" [^Writer w ^String s]
  (let [len (.length s)]
    (genPush "constant" len)
    (genCall "String.new" 1)
    ;;appendChar returns this
    (doseq [c s]
      (genPush "constant" (int c))
      (genCall "String.appendChar" 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genKeyword "" [^Writer w kw]
  (case kw
    ("null" "false")
    (genPush "constant" 0)
    "true"
    (do (genPush "constant" 0)
        (genAction "~"))
    "this"
    (genPush "pointer" 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genLiteral "" [^Writer w term]
  (let [{:keys [literal value]} term]
    (condp = literal
      "String"
      (genString w value)
      "int"
      (genPush w "constant" value)
      "keyword"
      (genKeyword w value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genVarr "" [^Writer w varr index]
  (let [info (if *class-level?*
               (or (get *funct-syms* varr)
                   (get *class-syms* varr))
               (get *funct-syms* varr))
        _ (if (nil? info)
            (c/trap! Exception
                     (str "bad varr: " varr)))
        {:keys [mtype index]} info
        mtype (if (= "field" mtype) "this" mtype)]
    (if (nil? index)
      (genPush w mtype index)
      (do
        (genExpr w index)
        (genPush w mtype index)
        (genAction w "+")
        (genPop w "pointer" 1)
        (genPush w "that" 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genInvoke "" [^Writer w call]
  (let [{:keys [target params]}
        call
        cnt (count params)]
    (doseq [p params]
      (genExpr w p))
    (genCall target cnt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genTerm "" [^Writer w term]
  (let [{:keys [literal varr index
                call
                unary term group]} term]
    (cond
      (some? literal)
      (genLiteral term)
      (some? call)
      (genInvoke w call)
      (some? group)
      (genExpr w group)
      (s/hgl? unary)
      (do
        (assert (some? term))
        (genTerm w term)
        (genAction w unary))
      (some? varr)
      (genVarr w varr index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genExpr "" [^Writer w expr]
  (doseq [tm (:output expr)
          :let [op? (= :OP (:tag tm))]]
    (genTerm w tm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genLet "" [^Writer w varr lhs rhs]
  (let [info (if *class-level?*
               (or (get *funct-syms* varr)
                   (get *class-syms* varr))
               (get *funct-syms* varr))
        _ (if (nil? info)
            (c/trap! Exception (str "bad varr: " varr)))
        {:keys [mtype index]} info
        mtype (if (= "field" mtype) "this" mtype)]
    ;;eval and set the right offset to the object-array
    (when (some? lhs)
      (genExpr w lhs)
      (genPush w mtype index)
      (genAction w "+"))
    ;;eval the statement
    (genExpr w rhs)
    ;;save the rhs result
    (genPop "temp" 0)
    ;;do the assignment
    (if (some? lhs)
      (do
        (genPop "pointer" 1)
        (genPush "temp" 0)
        (genPop "that" 0))
      (do
        (genPush "temp" 0)
        (genPop mtype index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genReturn "" [^Writer w expr]
  (if (some? expr)
    (genExpr w expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genAction "" [^Writer w op & [unary?]]
  (when-some
    [s (condp = op
         "+" "add"
         "-" (if unary? "neg" "sub")
         "*" "mult"
         "/" ""
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
(defn genPush "" [^Writer w what & more]
  (->> (s/strim (cs/join " " more))
         (str "push " what )
         (.write w))
    (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genCall "" [^Writer w func numArgs]
  (let [numArgs
        (+ (if (s/embeds? func ".") 1 0)
           numArgs)]
    (.write w (str "call " func " " numArgs))
    (nline w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genPop "" [^Writer w what & more]
  (->> (s/strim (cs/join " " more))
       (str "pop " what )
       (.write w))
  (nline w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genFunction "" [^Writer w, func, info]
  (genHeader w func info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genMethod "" [^Writer w, clazz, mtd, info]
  (genHeader w (str clazz "." mtd) info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genCtor "" [^Writer w, clazz, mtd, info]
  (genHeader w (str clazz "." mtd) info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



