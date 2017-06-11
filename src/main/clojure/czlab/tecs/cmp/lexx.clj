;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.cmp.lexx

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:use [clojure.walk])

  (:import [java.util.concurrent.atomic AtomicInteger]
           [czlab.basal.core GenericMutable]
           [java.io File LineNumberReader]
           [czlab.tecs.cmp JackParser]
           [java.net URL]
           [java.util Map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compilej "" [x] x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tokenj "" [furl]
  (with-open [inp (.openStream ^URL furl)]
    (let [p (JackParser. inp)]
      (c/prn!! "compiling file...")
      (.compileOneUnit p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- compFile ""
  [fp outDir]
  (c/prn!! "Processing file: %s" fp)
  (c/do-with
    [out (->> (io/as-url fp) tokenj compilej)]
    (let [nm (.getName ^File fp)
          t (io/file outDir
                     (str nm ".xml"))]
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


