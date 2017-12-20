(ns clj-advent-of-code-2017.day7
  (:gen-class))

(use 'clojure.java.io)
(use 'clojure.set)
(use 'clojure.pprint)
(require ['clojure.string :as 'str])

(defn get-lines [file]
  (str/split-lines (slurp file)))

(defn process-program [state line]
  (let [split-line (str/split line #"\s") weight (Integer/parseInt (subs (second split-line) 1 (- (count (second split-line)) 1)))]
    (if (= 2 (count split-line))
      (assoc state (first split-line) (hash-map :weight weight))
      (assoc
        state
        (first split-line)
        (hash-map
          :weight weight
          :relations (map #(str/replace % #"(,|\s)" "") (subvec split-line 3)))))))

(defn get-parents [current-state]
  (filter (fn [[k v]] (> (count v) 1)) current-state))

(defn get-root [current-state]
  (first (difference
     (set (keys current-state))
     (set (flatten (map (fn [[k v]] (:relations v)) (get-parents current-state)))))))

(defn get-children [current-state parent]
  (if (:relations (get current-state parent))
    (map get-children current-state (:relations (get current-state parent)))
    parent))

(def state (reduce process-program {} (get-lines "../../resources/day_7_input.txt")))

(defn sub-tower-weight [val node]
  (let [node-weight (:weight (get state node) 0) children (get-children state node)]
    (+ val (if (= (type children) clojure.lang.LazySeq)
             (reduce sub-tower-weight node-weight children)
             node-weight))))

(defn find-problem-level [state root]
  (loop [weights (map vector (map #(sub-tower-weight 0 %) (get-children state root)) (get-children state root))]
    (let [uneven-weight (ffirst (sort-by val (frequencies (map first weights))))
          problem-node (second (first (filter #(= uneven-weight (first %)) weights)))
          new-weight (map vector (map #(sub-tower-weight 0 %) (get-children state problem-node)) (get-children state problem-node))]
      (if (= 1 (count (set (map first new-weight))))
        weights
        (recur new-weight)))))

(defn fix-weight [state]
  (let [problem-level (find-problem-level state (get-root state))
        uneven-weight (ffirst (sort-by val (frequencies (map first problem-level))))
        expected-weight (first (second (sort-by val (frequencies (map first problem-level)))))
        problem-node (second (first (filter #(= uneven-weight (first %)) problem-level)))
        diff (- expected-weight uneven-weight )]
    (+ diff (:weight (get state problem-node)))))

(println "Part A:" (get-root state))
(println "Part B:" (fix-weight state))