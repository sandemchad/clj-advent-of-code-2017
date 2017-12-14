(ns clj-advent-of-code-2017.day13)
(use 'clojure.java.io)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn tick [[severity caught delay] firewall]
  (let [[index depth] firewall]
    (let [scan-pos (mod (+ delay index) (* (- depth 1) 2))]
      (if (zero? scan-pos)
        [(+ severity (apply * firewall)) true delay]
        [severity caught delay]))))

(def parsed-file
  (->> (get-lines "../../resources/day_13_input.txt")
       (map #(re-seq #"\d+" %))
       (flatten)
       (map #(Integer/parseInt %))
       (partition 2)))

(defn send-packet [delay]
  (reduce tick [0 false delay] parsed-file))

(time (println "Part A:" (first (send-packet 0))))
(time (println "Part B:" (some #(when (not (second (send-packet %))) %) (range))))