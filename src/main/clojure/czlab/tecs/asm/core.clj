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
            [clojure.string :as cs]
            [czlab.tecs.asm.fline :as fl]
            [czlab.tecs.asm.codes :as co])

  (:import [java.net URL]
           [java.io File LineNumberReader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private _builtins_
  (into {}
        (map (fn [[k v]] [k (int v)])
             {"SP"  0
              "LCL" 1
              "ARG" 2
              "THIS"  3
              "THAT"  4
              "SCREEN"  16384
              "KBD" 24576})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- builtinSymbols?? "" [syms s]
  (let [v1 (get _builtins_ s)
        [_ v2]
        (if (nil? v1)
          (re-matches #"^R([0-9]|1[0-5])" s))
        v (or (if (s/hgl? v2) (Integer/parseInt v2) v1) (int -1))]
    (if-not (neg? v)
      (assoc syms s v) syms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- findSymbols "" [lines jpts]
  (loop [[ln & more] lines
         syms {}
         rampos 16]
    (if (some? ln)
      (let [s (:symbol ln)
            ok? (and (fl/isAInstruction? ln)
                     (s/hgl? s))
            syms (if ok?
                   (builtinSymbols?? syms s) syms)
            [s' r']
            (if-not ok?
              [syms rampos]
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
        (recur more s' (long r'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeAInstruction "" [ln ^StringBuilder out]

  (let [^String v (:value @ln)
        w (co/bit16Word<>)
        ^String
        vs
        (if (s/hgl? v)
          (-> v Integer/parseInt Integer/toBinaryString cs/reverse) "")
        len (Math/min (int 16) (.length vs))]
    (doseq [pos (range len)]
      (->> (if (= \0 (.charAt vs pos)) 0 1)
           (co/setBit! w pos)))
    ;; its an a-instruction
    (co/setBit! w 15 0)
    (doto out
      (.append (str w))
      (.append "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- writeCInstruction "" [ln ^StringBuilder out]

  (let [{:keys [destCode
                jumpCode
                compCode]}
        @ln
        rom (co/bit16Word<>)]
    (doto rom
      ;; it's a c-instruction
      (co/setBit! 15 1)
      (co/setBit! 14 1)
      (co/setBit! 13 1))
    (let [dw
          (if (s/hgl? destCode)
            (co/code->destBits destCode)
            (co/bit16Word<>))
          jw
          (if (s/hgl? jumpCode)
            (co/code->jumpBits jumpCode)
            (co/bit16Word<>))
          cw
          (if (s/hgl? compCode)
            (co/code->compBits compCode)
            (co/bit16Word<>))
          w (-> rom
                (co/or?? cw) (co/or?? dw) (co/or?? jw))]
      (doto out
        (.append (str w))
        (.append "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- syntaxError! "" [ln]
  (c/trap! Exception
           (format "Syntax error near line: %s" (:line @ln))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass2 "" [lines]
  (c/sreduce<>
    #(cond
       (fl/isAInstruction? %2)
       (writeAInstruction %2 %1)
       (fl/isCInstruction? %2)
       (writeCInstruction %2 %1)
       :else %1)
    lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- pass1 "" [^URL furl]
  (with-open [inp (-> (.openStream furl)
                      io/reader
                      LineNumberReader. )]
    (loop [total []
           lbls []
           jpts {}
           mempos 0
           cur 1
           rdr inp
           line (.readLine rdr)]
      (if (nil? line)
        (do (findSymbols total jpts) total)
        (let [fl (fl/fileLine<> cur line)
              total (conj total fl)]
          (cond
            (:blank? @fl)
            (recur total lbls jpts mempos (inc cur) rdr (.readLine rdr))
            (fl/isLabel? fl)
            (recur total
                   (conj lbls fl)
                   jpts mempos (inc cur) rdr (.readLine rdr))
            (fl/isValidCodeLine? fl)
            (let
              [;store the position as raw ROM address
               _ (c/setf! fl :romPtr mempos)
               jpts (reduce
                      #(let [s (:label (deref %2))]
                         (when (c/in? %1 s) (syntaxError! %2))
                         (assoc %1 s mempos))
                      jpts lbls)
               lbls []
               mempos (inc mempos)]
              (recur total lbls jpts mempos
                     (inc cur) rdr (.readLine rdr)))
            :else
            (syntaxError! fl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanFile "" [fp fout]

  (c/prn!! "Processing file: %s" fp)
  (c/prn!! "Writing file: %s" fout)
  (->> (-> fp io/as-url pass1 pass2) (spit fout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -main "" [& args]

  (let [[src des & more] args]
    (if (and (some? src)
             (some? des)
             (empty? more))
      (try
        (scanFile (io/file src)
                  (io/file des))
        (catch Throwable e
          (.printStackTrace e)))
      (c/prn!! "Usage: asm <asm-file> <output-file>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

