(ns clj-advent-of-code-2017.day13)
(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn mirror-cycle [i]
  (cycle (concat (range i) (drop-last (drop 1 (reverse (range i)))))))

(defn tick [[severity caught delay] firewall]
  (let [[index depth] firewall]
    (let [scan-pos (nth (mirror-cycle depth) (+ delay index))]
      (if (= 0 scan-pos)
        [(+ severity (apply * firewall)) true delay]
        [severity caught delay]))))

(defn send-packet [delay]
  (->> (get-lines "../../resources/day_13_input.txt")
       (map #(re-seq #"\d+" %))
       (flatten)
       (map #(Integer/parseInt %))
       (partition 2)
       (reduce tick [0 false delay])))

(println "Part A:" (first (send-packet 0)))

(println (take 10 (filter #(even? %) (range))))

(println "Part B:" (reduce (fn [col v] (println "result" (first (send-packet v)) "delay" v) (if (not (second (send-packet v))) (reduced v) -1)) (filter #(even? %) (range))))