(ns clj-advent-of-code-2017.day4)
(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn uniq-words [line]
  (let [words (if (string? line) (str/split line #"\s") line)]
    (= (count words) (count (set words)))))

(defn sort-words [line]
  (let [words (str/split line #"\s")]
    (map #(apply str (sort %)) words)))

(let [lines (get-lines "../../resources/day_4_input.txt")]
  (println "Part A:" (count (filter uniq-words lines)))
  (println "Part B:" (count (filter uniq-words (map sort-words lines)))))