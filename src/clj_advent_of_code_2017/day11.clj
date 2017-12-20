(ns clj-advent-of-code-2017.day11
  (:require [clojure.string :as str]))

(def test-input ["se" "sw" "se" "sw" "sw"])

(def input (str/split (slurp "../../resources/day_11_input.txt") #","))

(def directions {"n"  [-1 0] "s" [1 0]
                 "w"  [0 -1] "e" [0 1]
                 "nw" [-0.5 -0.5] "ne" [-0.5 0.5]
                 "sw" [0.5 -0.5] "se" [0.5 0.5]})

(defn distance [pos]
  (reduce + (map #(Math/abs (Math/round %)) pos)))

(defn walk-hexes [[max-dis [x y]] move]
  (let [new-pos (map + [x y] (get directions move)) new-dis (distance new-pos)]
    [(if (> new-dis max-dis) new-dis max-dis) new-pos]))

(let [[max-distance pos] (reduce walk-hexes [0 [0 0]] input)]
  (println "Part A:" (distance pos))
  (println "Part B:" max-distance))
