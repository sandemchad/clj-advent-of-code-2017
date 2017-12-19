(ns clj-advent-of-code-2017.day6)
(use 'clojure.java.io)
(use '[clj-advent-of-code-2017.day10 :as day10 :only (rotate-a-seq)])
(require ['clojure.string :as 'str])

(defn read-file [file]
  (str/split (slurp file) #"\s"))

(def test-input [0 2 7 0])
(def input (mapv #(Integer/parseInt %) (read-file "../../resources/day_6_input.txt")))

(defn largest-index [vec]
  (let [largest (apply max vec)]
    (->> (map-indexed vector vec)
         (filter #(= largest (second %)))
         ffirst)))

(defn redistribute [vec i]
  (let [val (nth vec i) len (count vec) rotated (day10/rotate-a-seq (mod (inc i) len) (assoc vec i 0))]
    (apply vector (day10/rotate-a-seq (- (mod (inc i) len)) (map-indexed
                                                              (fn [i v]
                                                                (if (< i (mod val len))
                                                                  (+ (inc v) (quot val len))
                                                                  (+ v (quot val len))))
                                                              rotated)))))

(defn find-loop [vec]
  (loop [state vec prev-states [] depth 0]
    (if (.contains prev-states state)
      [state depth]
      (recur (redistribute state (largest-index state)) (conj prev-states state) (inc depth)))))

(let [result (find-loop input)]
  (println "Part A:" (second result))
  (println "Part B:" (second (find-loop (first result)))))