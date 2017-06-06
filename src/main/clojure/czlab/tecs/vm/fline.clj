;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.vm.fline

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable FileLineObj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanLine "" [obj]
  (let [^String line (:text @obj)
        pos (.indexOf line "//")
        line (if-not (neg? pos)
               (.substring line 0 pos) line)
        line (s/strim line)
        len (.length line)]
    (if (= 0 len)
      (c/setf! obj :blank? true)
      (let [tkns (.split line "[ \t\n\r]+")
            [p1 & pms] tkns
            p1  (s/lcase p1)]
        (c/setf! obj :action p1)
        (case p1
          ("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not")
          nil
          ("function" "call")
          (let [[a b & xs] pms]
            (c/setf! obj :func a)
            (c/setf! obj :args (Integer/parseInt b)))
          "return"
          (c/setf! obj :fret? true)
          ("push" "pop")
          (let [[a b & xs] pms]
            (c/setf! obj :segment a)
            (c/setf! obj :offset (Integer/parseInt b)))
          ("if-goto" "label" "goto")
          (let [[a & xs] pms]
            (c/setf! obj :label a))
          (c/trap! Exception (format "Unknown action: %s" line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileLine<> "" [lineNum lineStr]
  (doto (c/mutable<> FileLineObj
                     {:text (str lineStr)
                      :line lineNum
                      :blank? false
                      :action  nil
                      :segment nil
                      :offset nil
                      :label nil
                      :func nil
                      :args nil
                      :fret? false})
    scanLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


