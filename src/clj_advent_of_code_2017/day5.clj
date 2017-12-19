(ns clj-advent-of-code-2017.day5)
(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(def lines (mapv #(Integer/parseInt %) (get-lines "../../resources/day_5_input.txt")))

(defn recur-escape [inital-problem update-action]
  (loop [problem inital-problem offset 0 depth 0]
    (if (>= offset (count problem))
      depth
      (let [val (nth problem offset)]
        (recur (assoc problem offset ((update-action val) val 1)) (+ val offset) (inc depth))))))

(println "Part A:" (recur-escape lines (fn [_] +)))
(println "Part B:" (recur-escape lines (fn [val] (if (>= val 3) - +))))