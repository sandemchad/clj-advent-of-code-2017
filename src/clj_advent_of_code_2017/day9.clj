(ns clj-advent-of-code-2017.day9
  (:require [clojure.string :as str]))
(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(def test-input "<<<<>")
(def input (slurp "../../resources/day_9_input.txt"))

(defn find-groups [[q num] char]
  (if (= \{ char)
    [(conj q char) num]
    (if (and (> (count q) 0 ) (= \} char)) [(pop q) (+ (count q) num)] [q num])))

(let [no-nots (str/replace input #"(!.{1})" "") tidied (str/replace no-nots #"<[^>]*>" "") matched-garbage (re-seq #"<([^>]*)>" no-nots)]
  (println "Part A:" (second (reduce find-groups [() 0] (char-array tidied))))
  (println "Part B:" (reduce #(+ %1 (count (second %2))) 0 matched-garbage)))