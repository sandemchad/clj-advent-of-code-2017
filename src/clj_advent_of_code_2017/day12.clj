(ns clj-advent-of-code-2017.day12
  (:require [clojure.string :as str]))
(use 'clojure.data)
(use 'clojure.set)

(defn compact [coll]
  (remove nil? coll))

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn read-program [line]
  (let [[prog connections] (str/split line #" <-> ")]
    [prog (set (map str/trim (str/split connections #",")))]))

(defn generate-hash [data]
  (reduce #(apply assoc %1 (read-program %2) ) {} data))

(def state (generate-hash (get-lines "../../resources/day_12_input.txt")))

(defn find-connections [state checked-progs target-prog]
  (let [to-check (difference (get state target-prog) checked-progs)]
    (if (empty? to-check)
      checked-progs
      (reduce #(apply conj %1 %2) #{} (flatten (map #(find-connections state (conj checked-progs %) %) to-check))))
    ))

(defn find-groups [state checked-progs]
  (let [all-progs (set (keys state)) to-check (difference all-progs checked-progs) checking (first to-check)]
    (if (empty? to-check)
      0
      (+ 1 (find-groups state (apply conj checked-progs (find-connections state #{checking} checking)))))))


(println "Part A:" (count (find-connections state #{"0"} "0")))
(println "Part B:" (find-groups state #{}))
