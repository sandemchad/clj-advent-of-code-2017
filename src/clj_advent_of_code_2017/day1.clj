(defn pair? [pair]
  (let [a (Character/digit (first pair) 10) b (Character/digit (last pair) 10)]
    (if (= a b) a 0)
    )
  )

(defn sum-pairs [pairs]
  (reduce + (map pair? pairs))
  )

(defn get-half-pairs [input]
  (map-indexed
    (fn [i e]
      (let [step (quot (count input) 2)]
        [e (nth (cycle input) (+ i step))]
        ))
    input)
  )

(let [input (slurp "../../resources/day_1_input.txt")]
  (println "Part A:" (sum-pairs (partition 2 1 (concat input [(first input)]))))
  (println "Part B:" (sum-pairs (get-half-pairs input)))
  )