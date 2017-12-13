(ns clj-advent-of-code-2017.day2
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn diff [line]
  (let [p-line (map #(Integer/parseInt %) (str/split line #"\s"))]
    (- (apply max p-line) (apply min p-line))))

(defn mod-zero? [pair]
  (= 0 (apply mod (reverse (sort pair)))))

(defn divide-even [line]
  (let [p-line (map #(Integer/parseInt %) (str/split line #"\s"))]
    (let [correct-pair (first (filter mod-zero? (combo/combinations p-line 2)))]
      (apply quot (reverse (sort correct-pair))))))

(defn sum-diffs [lines]
  (reduce + (map diff lines)))

(defn sum-divs [lines]
  (reduce + (map divide-even lines)))

(println "Part A:" (sum-diffs (get-lines "../../resources/day_2_input.txt")))
(println "Part B:" (sum-divs (get-lines "../../resources/day_2_input.txt")))