(ns clj-advent-of-code-2017.day17)

(defn insert-at [col i val]
  (into [] (concat (subvec col 0 (+ 1 i)) [val] (subvec col (+ 1 i)))))

(defn spinlock [steps limit]
  (loop [buffer [0] i 0 depth 1]
    (if (= depth limit)
      buffer
      (let [forward-step (mod (+ i steps) depth)]
        (recur (insert-at buffer forward-step depth) (+ 1 forward-step) (inc depth))))))

(defn spin-for-zero [steps limit]
  (loop [after-0 0 i 0 depth 1]
    (if (= depth limit)
      after-0
      (let [forward-step (mod (+ i steps) depth)]
        (recur (if (= forward-step 0) depth after-0) (inc forward-step) (inc depth))))))

(let [result (spinlock 304 2018)]
  (println "Part A:" (nth result (+ 1 (.indexOf result 2017)))))

(println "Part B:" (spin-for-zero 304 50000000))