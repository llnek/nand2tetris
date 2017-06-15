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
           [java.util List Map]))

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
(defn- compilej "" [^ASTNode x]
  (let [node (toAST x)
        s (f/writeEdnStr node)]
    (c/prn!! s)
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tokenj "" [furl]
  (with-open [inp (.openStream ^URL furl)]
    (let [p (ASTGentor. inp)]
      (c/prn!! "Parsing file...")
      (ASTGentor/parseOneUnit))))

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



