(ns clj-advent-of-code-2017.day15)

(def a-start 703)
(def b-start 516)

(def a-factor 16807)
(def b-factor 48271)

(def a-div 4)
(def b-div 8)

(def div-by 2147483647)

(defn exec [val factor]
  (rem (* val factor) div-by))

(defn next-vals [[a-value b-value]]
  [(exec a-value a-factor) (exec b-value b-factor)])

(defn next-valid-val [val factor div]
  (loop [new-val val]
    (if (= 0 (mod new-val div))
      new-val
      (recur (exec new-val factor)))))

(defn next-vals-b [[a-value b-value]]
  [(next-valid-val (exec a-value a-factor) a-factor a-div)
   (next-valid-val (exec b-value b-factor) b-factor b-div)])

(defn bin-match [[a b]]
  (let [a (Integer/toString a 2) b (Integer/toString b 2)]
    (if (= (take-last 16 a) (take-last 16 b)) 1 0)))

(defn duel [starting-vals pairs compare-fn]
  (loop [matches 0 vals starting-vals depth 0]
    (if (= pairs depth)
      matches
      (let [new-vals (compare-fn vals)]
        (recur (+ (bin-match new-vals) matches) new-vals (inc depth))))))

(time (println "Part A:" (duel [a-start b-start] 40000000 next-vals)))
(time (println "Part B:" (duel [a-start b-start] 5000000 next-vals-b)))