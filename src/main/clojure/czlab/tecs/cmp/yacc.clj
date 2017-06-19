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

  (:require [czlab.tecs.cmp.cgen :as cg]
            [czlab.basal.format :as f]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:use [clojure.walk])

  (:import [java.io Writer StringWriter File LineNumberReader]
           [java.util.concurrent.atomic AtomicInteger]
           [czlab.tecs.p11 Node ASTNode ASTGentor]
           [czlab.basal.core GenericMutable]
           [java.net URL]
           [java.util Stack ArrayList List Map]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doFuncVar ""
  [^Writer w {:keys [attrs statements] :as node}]
  (let [{:keys [args name type
                qualifier vars]}
        attrs
        ctor? (= "constructor"
                 qualifier)
        func? (= "function"
                 qualifier)
        params (atom {})
        locals (atom {})
        args (partition 2 args)
        vars (partition 2 vars)
        gist {:arg-count (count args)
              :isMethod? (not func?)
              :isCtor? ctor?
              :type type
              :name name
              :var-count (count vars)}]
    (.set cg/arg-cntr 0)
    (.set cg/var-cntr 0)
    (if-not func?
      (swap! params
             assoc
             "this" {:dtype cg/*class-name*
                     :mtype "field"
                     :index (.getAndIncrement cg/arg-cntr)}))
    (doseq [[t n] args]
      (swap! params
             assoc
             n
             {:dtype t
              :mtype "argument"
              :index (.getAndIncrement cg/arg-cntr)}))
    (doseq [[t n] vars]
      (swap! locals
             assoc
             n
             {:dtype t
              :mtype "local"
              :index (.getAndIncrement cg/var-cntr)}))
    (binding
      [cg/*class-level?* (not func?)
       cg/*class-ctor?* ctor?
       cg/*func-info* gist
       cg/*local-syms* @locals
       cg/*arg-syms* @params]
      (cg/genFnHeader w gist)
      (cg/genFnBody w statements))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doClassVar "" [statics
                      fields
                      {:keys [attrs] :as node}]
  (let [{:keys [type
                vars
                qualifier]} attrs
        [ctx ^AtomicInteger ctr]
        (condp = qualifier
          "static" [statics cg/static-cntr]
          "field"  [fields cg/field-cntr]
          (c/trap!
            Exception
            (str "bad qualifier: " qualifier)))
        dft {:mtype qualifier
             :dtype type}]
    (if (string? vars)
      (->> {:index (.getAndIncrement ctr)}
           (merge dft)
           (swap! ctx assoc vars))
      (doseq [s (seq vars)]
        (->> {:index (.getAndIncrement ctr)}
             (merge dft)
             (swap! ctx assoc s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compilej "" [^Writer w root]
  (let [statics (atom {})
        fields (atom {})
        [vs fs]
        (->> (:children root)
             (split-with
               #(= :ClassVarDec (:tag %))))]
    (doseq [c vs]
      (doClassVar statics fields c))
    (binding
      [cg/*static-syms* @statics
       cg/*class-syms* @fields
       cg/*class-name* (get-in root
                               [:attrs :name])]
      (doseq [f fs]
        (assert (= :SubroutineDec (:tag f)))
        (doFuncVar w f)))
    root))

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
  (let [ast (->> (io/as-url fp) tokenj toAST)
        w (StringWriter.)
        nm (.getName ^File fp)
        v (io/file outDir (str nm ".vm"))
        j (io/file outDir (str nm ".clj"))]
    (c/prn!! "Writing file: %s" j)
    (spit j (f/writeEdnStr ast))
    (compilej w ast)
    (c/prn!! "Writing file: %s" v)
    (spit v (.toString w))))

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
          (println (str "Error: " (.getMessage e)))))
      (c/prn!! "Usage: cmp <jack-file> <out-dir>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



