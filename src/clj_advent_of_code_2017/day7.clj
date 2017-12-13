(ns clj-advent-of-code-2017.day7
  (:gen-class))

(use 'clojure.java.io)
(use 'clojure.set)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn process-program [state line]
  (let [split-line (str/split line #"\s")]
    (if (= 2 (count split-line))
      (assoc state (first split-line) (hash-map :weight (second split-line)))
      (assoc
        state
        (first split-line)
        (hash-map
          :weight (second split-line)
          :relations (map #(str/replace % #"(,|\s)" "") (subvec split-line 3)))))))

(defn get-parents [current-state]
  (filter (fn [[k v]] (> (count v) 1)) current-state))

(defn get-root [current-state]
  (first (difference
     (set (keys current-state))
     (set (flatten (map (fn [[k v]] (:relations v)) (get-parents current-state)))))))

(defn update-relations [new-state node]
  (println node))

(defn get-children [current-state parent]
  (if (:relations (get current-state parent))
    (map get-children current-state (:relations (get current-state parent)))
    parent))

(def state (reduce process-program {} (get-lines "../../resources/day_7_test.txt")))
(println state)
(println (count state))
(println (get-parents state))

(println (reduce update-relations state (get-parents state)))
(println "ffs" (get-children state (get-root state)))