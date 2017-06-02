;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Represent a line in the ASM file."
      :author "Kenneth Leung"}

  czlab.tecs.asm.fline

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- syntaxError! "" [obj]
  (c/trap! Exception
           (format "Syntax error near line: %s" (:line @obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol FileLine
  ""
  (isCInstruction? [_] "")
  (isAInstruction? [_] "")
  (isLabel? [_] "")
  (isValidCodeLine? [_] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanLine "" [obj]
  (let [^String line (:text @obj)
        pos (.indexOf line "//")
        line (if-not (neg? pos)
               (.substring line 0 pos) line)
        line (s/strim line)
        len (.length line)]
    (cond
      (= 0 len)
      (c/setf! obj :blank? true)
      (.startsWith line "@")
      (let
        [^String
         s1 (if (= 1 len)
              (syntaxError! obj)
              (.substring line 1))]
        (c/setf! obj
                 (if-not (Character/isDigit (.charAt s1 0))
                   :symbol :value) s1)
        (c/setf! obj :instKey :A))
      (and (.startsWith line "(")
           (.endsWith line ")"))
      (let [pos (.lastIndexOf line ")")]
        (c/setf! obj
                 :label
                 (.substring line 1 pos)))
      :else
      (let [pos (.indexOf line ";")
            line
            (if (pos? pos)
              (do (c/setf! obj
                           :jumpCode
                           (.substring line (inc pos)))
                  (.substring line 0 pos))
              line)
            pos (.indexOf line "=")
            line
            (if (pos? pos)
              (do (c/setf! obj :destCode
                           (.substring line 0 pos))
                  (.substring line (inc pos)))
              line)]
        (c/setf! obj :compCode line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable FileLineObj
  FileLine
  (isCInstruction? [me] (and (not (.isLabel? me))
                             (not (:blank? @me))
                             (not (.isAInstruction? me))))
  (isAInstruction? [me] (and (not (.isLabel? me))
                             (not (:blank? @me))
                             (= :A (:instKey @me))))
  (isLabel? [me] (s/hgl? (:label @me)))
  (isValidCodeLine? [me] (or (.isAInstruction? me)
                             (.isCInstruction? me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileLine<> "" [lineNum lineStr]
  (doto
    (c/mutable<> FileLineObj
                 {:text (str lineStr)
                  :line lineNum
                  :blank? false
                  :instKey nil
                  :symbol nil
                  :label nil
                  :value nil
                  :jumpCode nil
                  :destCode nil
                  :compCode nil
                  :romPtr -1})
    scanLine ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

