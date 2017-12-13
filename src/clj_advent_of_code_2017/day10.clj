(ns clj-advent-of-code-2017.day10
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [cl-format]]))
(use 'clojure.java.io)

(defn get-lengths [file]
  (map #(Integer/parseInt %) (str/split (slurp file) #",")))

(def lengths (apply vector (get-lengths "../../resources/day_10_input.txt")))
(def element-list (apply vector (range 256)))

(defn rotate-a-seq [n lat]
  (let [x (count lat)
        a (if (neg? n) (reverse (take (mod (* n -1) x) (reverse lat)))
                       (drop (mod n x) lat))
        b (if (neg? n) (take (mod (+ x n) x) lat)
                       (take (mod n x) lat))]
    (concat a b)))

(defn reverse-subset [col start end]
  (let [rotated (apply vector (rotate-a-seq start col)) end (- end start)]
    (rotate-a-seq (- start) (concat (reverse (subvec rotated 0 end)) (subvec rotated end)))))

(defn steps [lens [i pos e-list]]
  (reduce (fn [[i pos e-list] lens]
            [(mod (inc i) 256) (mod (+ pos (+ lens i)) 256) (reverse-subset e-list pos (+ pos lens))])
          [i pos e-list]
          lens))

(println "Part A:" (->> [0 0 (range 256)]
                        (steps lengths)
                        last
                        (take 2)
                        (apply *)))

(def lengths (map #(int %) (char-array (slurp "../../resources/day_10_input.txt"))))

(println "Part B:" (->> [0 0 (range 256)]
                        (iterate (partial steps (concat lengths [17, 31, 73, 47, 23])))
                        (drop 64)
                        first
                        last
                        (partition 16)
                        (map #(reduce bit-xor %))
                        (map #(cl-format nil "~2,'0x" %))
                        (apply str)))