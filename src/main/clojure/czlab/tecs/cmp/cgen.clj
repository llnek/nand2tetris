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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private genHeader "" [w fname info]
  `(.write ~w (str ~fname " " (count (:args ~info)))))


(defmacro ^:private nline "" [w] `(.write ~w "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genExpr "" [w expr])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genArrayAccess "" [^Writer w cst fst varr expr]
  (let [info (or (get fst varr)
                 (get cst varr))
        _ (if (nil? info)
            (c/trap! Exception
                     ("bad varr: " + varr)))
        t (:mtype info)
        i (:index info)]
    (if (= "field" t)
      (genPush w "this" i)
      (genPush w t i))
    (genExpr w expr)
    (genAction w "+")
    (genPop "pointer" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


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



