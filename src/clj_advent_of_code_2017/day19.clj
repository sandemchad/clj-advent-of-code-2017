(ns clj-advent-of-code-2017.day19
  (:require [clojure.string :as str]))

(defn build-matrix [file]
  (mapv #(str/split % #"") (apply vector (str/split-lines (slurp file)))))

(def matrix (build-matrix "../../resources/day_19_input.txt"))

(defn find-start []
  (.indexOf (first matrix) "|"))

(defn neighbors [[i j]]
  (let [x ((juxt inc inc identity dec dec dec identity inc) i)
        y ((juxt identity inc inc inc identity dec dec dec) j)]
    (map vector x y)))

(defn find-neighbour [pos dir]
  (last (last (filter #(and (identity (first %)) (not= (first %) " "))
                 (map #(vector (get-in matrix %) %)
                      (filter #(not= % (map - pos dir)) (neighbors pos)))))))

(defn handle-pos [state pos direction word-path]
  (cond
    (or (= state "|") (= state "-")) (list (map + pos direction) direction word-path)
    (= state "+") (let [next-pos (find-neighbour pos direction)]
                    (list next-pos (mapv - next-pos pos) word-path))
    :else (list (map + pos direction) direction (conj word-path state))))

(defn follow-line [start-point]
  (loop [pos start-point direction [1 0] word-path [] steps 0]
    (let [current-state (get-in matrix pos)]
      (if (or (nil? current-state) (= current-state " "))
        [word-path steps]
        (let [[p d w] (handle-pos current-state pos direction word-path)]
          (recur p d w (inc steps)))))))

(let [[path steps] (follow-line [0 (find-start)])]
  (println "Part A:" (apply str path))
  (println "Part B:" steps))
