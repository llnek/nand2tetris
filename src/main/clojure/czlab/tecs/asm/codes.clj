;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.tecs.asm.codes

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:import [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol Bit16Word
  ""
  (setBit! [_ k v] "")
  (getBit [_ k] "")
  (setBits! [_k m] "")
  (or?? [_ w] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ensurePos! "" [pos]
  (if (or (< pos 0)
          (> pos 15))
    (c/trap! Exception (format "Index out of range: %s" pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ensureVal! "" [v]
  (if-not (or (= v 0)
              (= v 1))
    (c/trap! Exception (format "Bad binary value: %s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstraction for a 16-bit word.
(declare bit16Word<>)
(c/decl-mutable Bit16WordObj
  Bit16Word
  (setBit! [me pos value]
    (ensurePos! pos)
    (ensureVal! value)
    (aset-int ^ints (:word @me) pos (int value)))
  (getBit [me pos]
    (ensurePos! pos)
    (aget ^ints (:word @me) pos))
  (setBits! [me m]
    (doseq [[k v] m]
      (.setBit! me k v)))
  (or?? [me other]
    (let [ow (:word @other)
          mw (:word @me)]
      (c/do-with [r (bit16Word<>)]
        (doseq [pos (range 16)]
          (if (or (= 1 (aget ^ints mw pos))
                  (= 1 (aget ^ints ow pos)))
            (setBit! r pos 1))))))
  Object
  (toString [me]
    (let [w (:word @me)
          b (s/strbf<>)]
      ;; need to reverse it - MSB to appear first
      (doseq [pos (range 15 -1 -1)]
        (.append b (aget ^ints w pos)))
      (.toString b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn bit16Word<> "" []
  (c/mutable<> Bit16WordObj {:word (int-array 16 0)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn code->jumpBits "" [code]
  (let [code (-> code s/strim s/ucase)]
    (c/do-with [w (bit16Word<>)]
      ;; bit= 2, 1, 0
      (->> (case code
             "JGT" { 2 0, 1 0, 0 1 }
             "JEQ" { 2 0, 1 1, 0 0 }
             "JGE" { 2 0, 1 1, 0 1 }
             "JLT" { 2 1, 1 0, 0 0 }
             "JNE" { 2 1, 1 0, 0 1 }
             "JLE" { 2 1, 1 1, 0 0 }
             "JMP" { 2 1, 1 1, 0 1 }
             (c/trap! Exception (format "Bad jump code %s" code)))
           (setBits! w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn code->destBits "" [code]
  (let [code (-> code s/strim s/ucase)]
    (c/do-with [w (bit16Word<>)]
      ;; set bits 5,4,3
      (->> (case code
             "M"   { 5 0, 4 0, 3 1 }
             "D"   { 5 0, 4 1, 3 0 }
             "MD"  { 5 0, 4 1, 3 1 }
             "A"   { 5 1, 4 0, 3 0 }
             "AM"  { 5 1, 4 0, 3 1 }
             "AD"  { 5 1, 4 1, 3 0 }
             "AMD" { 5 1, 4 1, 3 1 }
             (c/trap! Exception (format "Bad dest code %s" code)))
           (setBits! w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn code->compBits "" [code]
  (let [code (-> code s/strim s/ucase)
        [code' wasMNotA?]
        (if-not (neg? (.indexOf code "M"))
          ;; if the actual register affected is M, then we flag it, but
          ;; fake it into A register for comparison below
          [(.replace code "M" "A") true]
          [code false])]
    (c/do-with [w (bit16Word<>)]
      ;; bits 11,10,9,8,7,6
      (->> (case code'
             "0"   { 11 1, 10 0, 9 1, 8 0, 7 1, 6 0 }
             "1"   { 11 1, 10 1, 9 1, 8 1, 7 1, 6 1 }
             "-1"  { 11 1, 10 1, 9 1, 8 0, 7 1, 6 0 }
             "D"   { 11 0, 10 0, 9 1, 8 1, 7 0, 6 0 }
             "A"   { 11 1, 10 1, 9 0, 8 0, 7 0, 6 0 }
             "!D"  { 11 0, 10 0, 9 1, 8 1, 7 0, 6 1 }
             "!A"  { 11 1, 10 1, 9 0, 8 0, 7 0, 6 1 }
             "-D"  { 11 0, 10 0, 9 1, 8 1, 7 1, 6 1 }
             "-A"  { 11 1, 10 1, 9 0, 8 0, 7 1, 6 1 }
             "D+1" { 11 0, 10 1, 9 1, 8 1, 7 1, 6 1 }
             "A+1" { 11 1, 10 1, 9 0, 8 1, 7 1, 6 1 }
             "D-1" { 11 0, 10 0, 9 1, 8 1, 7 1, 6 0 }
             "A-1" { 11 1, 10 1, 9 0, 8 0, 7 1, 6 0 }
             "D+A" { 11 0, 10 0, 9 0, 8 0, 7 1, 6 0 }
             "D-A" { 11 0, 10 1, 9 0, 8 0, 7 1, 6 1 }
             "A-D" { 11 0, 10 0, 9 0, 8 1, 7 1, 6 1 }
             "D&A" { 11 0, 10 0, 9 0, 8 0, 7 0, 6 0 }
             "D|A" { 11 0, 10 1, 9 0, 8 1, 7 0, 6 1 }
             (c/trap! Exception (format "Bad comp code %s" code)))
           (setBits! w))
      (if wasMNotA?
        (setBit! w 12 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


