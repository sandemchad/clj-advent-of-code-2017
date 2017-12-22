(ns clj-advent-of-code-2017.day16
  (:require [clojure.string :as str]))


(defn get-lines [file]
  (str/split (slurp file) #","))

(def programs (apply vector (map char (range 97 (+ 97 16)))))

(defn spin [col param]
  (let [x (Integer/parseInt param)]
    (apply vector (concat (take-last x col) (take (- (count col) x) col)))))

(defn exchange [col params]
  (let [[a b] (map #(Integer/parseInt %) (str/split params #"/")) a-val (nth col a) b-val (nth col b)]
    (assoc (assoc col a b-val) b a-val)))

(defn partner [col params]
  (let [[prog-a _ prog-b] (seq params) a (.indexOf col prog-a) b (.indexOf col prog-b)]
    (assoc (assoc col a prog-b) b prog-a)))

(defn parse-method [char]
  (get {\s spin \x exchange \p partner} char))

(defn parse [col s]
  (let [fn-code (first s) params (.substring s 1)]
    ((parse-method fn-code) col params)))

(defn loop-parse [col s]
  (let [new-col (reduce parse col s)]
    (loop [new-col new-col result-seq [col]]
      (if (= col new-col)
        result-seq
        (recur (reduce parse new-col s) (conj result-seq new-col))))))

(let [loop-seq (loop-parse programs (get-lines "../../resources/day_16_input.txt"))]
  (println "Part A:" (apply str (nth loop-seq 1)))
  (println "Part B:" (apply str (nth loop-seq (mod 1000000000 (count loop-seq))))))