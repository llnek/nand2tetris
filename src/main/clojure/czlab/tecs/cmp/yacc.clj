;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.cmp.yacc

  (:require [czlab.basal.format :as f]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:use [clojure.walk])

  (:import [java.io StringWriter File LineNumberReader]
           [java.util.concurrent.atomic AtomicInteger]
           [czlab.tecs.p11 Node ASTNode ASTGentor]
           [czlab.basal.core GenericMutable]
           [java.net URL]
           [java.util Stack ArrayList List Map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^AtomicInteger static-cntr (AtomicInteger.))
(def ^:private ^AtomicInteger class-cntr (AtomicInteger.))
(def ^:private static-symbols (GenericMutable. {}))
(def ^:private class-symbols (GenericMutable. {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- printj "" [^ASTNode x]
  (let [w (StringWriter.)
        ;_ (.dumpXML x w)
        _ (.dumpEDN x w)]
    (c/do-with
      [s (.toString w)] (c/prn!! s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare toAST)
(defn- convObj "" [obj]
  (cond
    (c/ist? Node obj)
    (toAST obj)
    (c/ist? List obj)
    (mapv #(convObj %) obj)
    :else (str obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toAST "" [^ASTNode node]
  (let [v (.jjtGetValue node)
        k (.toString node)
        stag (keyword k)
        props (.-props node)
        nested (.-nested node)
        hasC? (pos? (.jjtGetNumChildren node))
        ctx (GenericMutable. {:tag stag})]
    (when (some? v)
      (->> (convObj v)
           (c/setf! ctx :value)))
    ;;handle props
    (when (pos? (.size props))
      (->> (c/preduce<map>
             #(let [[k v] %2
                    kee (keyword k)]
                (assoc! %1
                        kee (convObj v))) props)
           (c/setf! ctx :attrs)))
    ;;handle nested
    (when (pos? (.size nested))
      (c/preduce<map>
        (fn [sum [k v]]
           (->>
             (convObj v)
             (c/setf! ctx (keyword k))) sum) nested))
    (when hasC?
      (loop [len (.jjtGetNumChildren node)
             pos 0
             nodes []]
        (if-not (< pos len)
          (c/setf! ctx :children nodes)
          (->> (toAST (.jjtGetChild node pos))
               (conj nodes)
               (recur len (inc pos))))))
    (deref ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare eval-expr eval-term eval-array-access eval-var)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- speek "" [^Stack s]
  (if-not (.empty s) (.peek s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- spop "" [^Stack s]
  (if-not (.empty s) (.pop s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-unary "" [ctx term]
  (eval-term term))
  ;;(eval-unary (:unary ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-subr-call ""
  [ctx {:keys [target params] :as node}]
  (let [[z m](.split ^String target "\\.")]
    (doseq [e params]
      (eval-expr ctx e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-term ""
  [{:keys [literal group call
           unary varr index] :as node}]
  (let [ctx {}]
    (cond
      (some? unary)
      nil
      (some? literal)
      nil
      (some? varr)
      (if (some? index)
        (eval-array-access ctx index)
        (eval-var ctx varr))
      (some? group)
      (eval-expr ctx group)
      (some? call)
      (eval-subr-call ctx call)
      :else nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-expr "" [ctx expr]
  (doseq [c (:output expr)
          :let [tag (:tag c)]]
    (condp = tag
      :Term nil
      :OP nil
      (c/trap! Exception "bad expr"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-do "" [{:keys [call] :as stmt}]
  (eval-subr-call {} call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-while "" [stmt] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-if "" [stmt] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-return "" [stmt] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-array-access "" [ctx expr] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-var "" [ctx varr] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- eval-let "" [{:keys [varr lhs rhs] :as stmt}]
  (eval-expr {} rhs)
  (if (some? lhs)
    (eval-array-access {} lhs) (eval-var {} varr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doFuncVar "" [{:keys [attrs statements] :as node}]
  (let [{:keys [args name type
                qualifier vars]}
        attrs
        pc (AtomicInteger.)
        lc (AtomicInteger.)
        px (GenericMutable. {})
        lx (GenericMutable. {})
        args (partition 2 args)
        vars (partition 2 vars)]
    (c/prn!! "doFuncVar %s" name)
    (doseq [s statements
            :let [t (:tag s)]]
      (cond
        (= :WhileStatement t)
        (eval-while s)
        (= :LetStatement t)
        (eval-let s)
        (= :DoStatement t)
        (eval-do s)
        (= :IfStatement t)
        (eval-if s)
        (= :ReturnStatement t)
        (eval-return s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doClassVar "" [{:keys [attrs] :as node}]
  (let [{:keys [type
                vars
                qualifier]} attrs
        dft {:type type}
        [ctx ^AtomicInteger ctr]
        (condp = qualifier
          "static" [static-symbols static-cntr]
          "field"  [class-symbols class-cntr]
          (c/trap! Exception "bad qualifier"))]
    (if (string? vars)
      (->> {:index (.getAndIncrement ctr)}
           (merge dft)
           (c/setf! ctx vars))
      (doseq [s (seq vars)]
        (->> {:index (.getAndIncrement ctr)}
             (merge dft)
             (c/setf! ctx s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compilej "" [^ASTNode x]
  (let [root (toAST x)
        s (f/writeEdnStr root)]
    (doseq [c (:children root)
            :let [tag (:tag c)]]
      (cond
        (= :ClassVarDec tag)
        (doClassVar c)
        (= :SubroutineDec tag)
        (doFuncVar c)))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tokenj "" [furl]
  (with-open [inp (.openStream ^URL furl)]
    (let [p (ASTGentor. inp)]
      (c/prn!! "Parsing file...")
      (.parseOneUnit p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compFile ""
  [fp outDir]
  (c/prn!! "Processing file: %s" fp)
  (c/do-with
    [out (->> (io/as-url fp) tokenj compilej)]
    (let [nm (.getName ^File fp)
          t (io/file outDir
                     (str nm ".clj"))]
      (c/prn!! "Writing file: %s" t)
      (spit t out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanDir "" [dir out]
  (doseq [f (i/listFiles dir ".jack")]
    (compFile f out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]

  (let [[src des & more] args]
    (if (and (s/hgl? src)
             (s/hgl? des)
             (empty? more))
      (try
        (let [f (io/file src)
              d (io/file des)]
          (if (.isDirectory f)
            (scanDir f d)
            (compFile f d)))
        (catch Throwable e
          (.printStackTrace e)))
      (c/prn!! "Usage: cmp <jack-file> <out-dir>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



