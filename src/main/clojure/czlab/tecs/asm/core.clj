;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "An assembler for TECS.  Reads in a asm file and outputs a .hack binary file."
      :author "Kenneth Leung"}

  czlab.tecs.asm.core

  (:require [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [clojure.java.io :as io]
            [czlab.basal.str :as s]
            [clojure.string :as cs])

  (:import [java.io File LineNumberReader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _builtins_
  {"SP" 0
   "LCL" 1
   "ARG" 2
   "THIS"  3
   "THAT"  4
   "SCREEN"  16384
   "KBD" 24576})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- builtinSymbols "" [syms s]
  (let [v1 (get _builtins_ s)
        [_ v2]
        (if (nil? v1)
          (re-matches #"^R([0-9]|1[0-5])" s))
        v (or (if (s/hgl? v2) (Long/parseLong v2) v1) -1)]
    (if-not (neg? v)
      (assoc syms s v) syms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- findSymbols "" [lines jpts]
  (loop [[ln & more] lines
         syms {}
         rampos 16]
    (if (some? ln)
      (let [a? (:instructionA? ln)
            s (:symbol ln)
            syms (if (and a? (s/hgl? s))
                   (builtinSymbols?? syms s) syms)
            [s' r']
            (when (and a? (s/hgl? s))
              (cond
                ;; if symbol refers to a label, get the label address
                (c/in? jpts s)
                (do (c/setf! ln :value (str (get jpts s)))
                    [syms rampos])
                ;; if symbol exists already, get its address
                (c/in? syms s)
                (do (c/setf! ln :value (str (get syms s)))
                    [syms rampos])
                ;; new symbol, add it and save its RAM address
                :else
                (do (c/setf! ln :value (str rampos))
                    [(assoc syms s rampos) (inc rampos)])))]
        (recur more s' r')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeAInstruction "" [ln ^StringBuilder out]

  (let [w (bit16Word<>)
        v (:value ln)
        vs
        (if (s/hgl? v)
          (-> v Integer/parseInt Integer/toBinaryString .reverse) "")
        len (Math/min 16 (.length vs))]
    (doseq [pos (range len)]
      (->> (if (= '0' (.charAt vs pos)) 0 1)
           (.setBit w pos)))
    ;; its an a-instruction
    (.setBit w 15 0)
    (doto out
      (.append (str w))
      (.append "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeCInstruction "" [ln ^StringBuilder out]

  (let [{:keys [destCode
                jumpCode
                compCode]}
        ln
        rom (bit16Word<>)]
    (doto rom
      ;; it's a c-instruction
      (.setBit 15 1)
      (.setBit 14 1)
      (.setBit 13 1))
    (let [dw
          (if (s/hgl? destCode)
            (doto (destOut<>)
              (.convertToBits destCode))
            (bit16Word<>))
          jw
          (if (s/hgl? jmpCode)
            (doto (jumpCode<>)
              (.convertToBits jmpCode))
            (bit16Word<>))
          cw
          (if (s/hgl? compCode)
            (doto (compCode<>)
              (.convertToBits compCode))
            (bit16Word<>))
          s (-> rom
                (.or cw) (.or dw) (.or jw) str)]
      (doto out
        (.append s)
        (.append "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- syntaxError! "" [ln]
  (trap! Exception
         "Syntax error near line: %s" (:fileline ln)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass2 "" [lines]
  ;;def me=this, out= new StringBuilder(1024);
  (c/sreduce
    #(cond
       (:instructionA? %2)
       (do (writeAInstruction %2 %1) %1)
       (:instructionC? %2)
       (do (writeCInstruction %2 %1) %1)
       :else %1)
    lines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass1 "" [^URL furl]
  ;;def me=this, fl, s, cur=0, mempos=0, lbls=[], jpts=[:];
  (with-open [inp (-> (.openStream furl)
                      io/reader
                      LineNumberReader. )]
    (loop [total []
           lbls []
           jpts {}
           rdr inp
           cur 1
           line (.readLine rdr)]
      (if (nil? line)
        (do
          (findSymbols total jpts)
          total)
        (let [fl (fileLine<> cur line)]
          (cond
            (:blank? fl)
            (recur total lbls jpts rdr (inc cur) (.readLine rdr))
            (:label? fl)
            (recur (conj total fl)
                   (conj lbls fl)
                   jpts rdr (inc cur) (.readLine rdr))
            (:validCodeLine? fl)
            (let
              [;store the position as raw ROM address
               _ (c/setf! fl :rompos mempos)
               jpts (reduce
                      #(let [s (:label %2)]
                         (when (c/in? %1 s) (syntaxError! %2))
                         (assoc %1 s mempos))
                      jpts lbls)
               lbls []
               mempos (inc mempos)]
              (recur ))
            :else
            (syntaxError! fl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanFile "" [^File fp ^File fout]

  (c/prn!! "Processing file: %s" (.getName fp))
  (let [lines (pass1 fp [])
        out (pass2 lines)]
    (c/prn!! "Writing file: %s" (.getName fout))
    (spit fout out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]

  (let [[src des & more] args]
    (if (and (some? src)
             (some? des)
             (empty? more))
      (try
        (scanFile (io/file src) (io/file des))
        (catch Throwable e
          (.printStackTrace e)))
      (c/prn!! "Usage: asm <asm-file> <output-file>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

