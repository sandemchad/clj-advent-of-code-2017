(ns clj-advent-of-code-2017.day20
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (zipmap
    (map last (re-seq #"([a-zA-Z]+)=" line))
    (map #(apply vector %) (partition 3 (map #(Integer/parseInt %) (re-seq #"-?\d+" line))))))

(defn get-lines [file]
  (map parse-line (str/split-lines (slurp file))))

(defn tick [particle]
  (let [{p "p" v "v" a "a"} particle v (mapv + v a) p (mapv + p v)]
    {"p" p "v" v "a" a}))

(defn closest [state]
  (ffirst (sort-by last (map-indexed #(conj [%1] (reduce + (map (fn [v] (Math/abs v)) (get %2 "p")))) state))))

(defn duplicate-pos [state]
  (map first
       (remove (comp #{1} val)
               (frequencies (map #(second (first %)) state)))))

(defn remove-collisions [state]
  (if (apply distinct? (map #(get % "p") state))
    state
    (remove (fn [{p "p"}] (some #{p} (duplicate-pos state))) state)))

(defn part-a [input n]
  (loop [state input depth 0]
    (if (= n depth)
      (closest state)
      (recur (map tick state) (inc depth)))))

(defn part-b [input n]
  (loop [state input depth 0]
    (let [new-state (remove-collisions (map tick state))]
      (if (= n depth)
        (count state)
        (recur new-state (inc depth))))))

(println "Part A:" (part-a (get-lines "../../resources/day_20_input.txt") 500))

(println "Part B:" (part-b (get-lines "../../resources/day_20_input.txt") 500))

