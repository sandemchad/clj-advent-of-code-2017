(ns clj-advent-of-code-2017.day8)
(require ['clojure.string :as 'str])

(defn get-lines []
  (map #(str/split % #"\s") (str/split-lines (slurp "../../resources/day_8_input.txt"))))

(defn process-line [regs [target-reg action val _ cond-reg cond-action cond-val]]
  (let [act-method (if (= action "inc") + -)
        comp-oper (if (= cond-action "!=") "not=" cond-action)
        highest-value (apply max (vals regs))
        new-regs (if (> highest-value (get regs "highestvalue")) (assoc regs "highestvalue" highest-value) regs)]
    (if ((resolve (symbol comp-oper)) (get regs cond-reg 0) (Integer/parseInt cond-val))
      (assoc new-regs target-reg (act-method (get regs target-reg 0) (Integer/parseInt val)))
      new-regs))
  )

(let [result (reduce process-line {"highestvalue" 0} (get-lines))]
  (println "Part A:" (apply max (vals (dissoc result "highestvalue"))))
  (println "Part A:" (get result "highestvalue")))