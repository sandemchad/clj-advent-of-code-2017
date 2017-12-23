(ns clj-advent-of-code-2017.day18
  (:require [clojure.string :as str]))

(defn read-input [file]
  (map #(str/split % #"\s") (str/split-lines (str/replace (slurp file) #"set|mod" {"set" "d18set" "mod" "d18mod"}))))

(defn translate-val [registers val]
  (if (or (= java.lang.Long (type val)) (= java.lang.Integer (type val)))
    val
    (if (re-matches #"-?\d+" val) (Integer/parseInt val) (get registers val 0))))

(defn d18set [registers [key val]]
  (assoc registers key (translate-val registers val)))

(defn d18mod [registers [key val]]
  (d18set registers [key (mod (translate-val registers key) (translate-val registers val))]))

(defn add [registers [key val]]
  (assoc registers key (+ (translate-val registers key) (translate-val registers val))))

(defn mul [registers [key val]]
  (assoc registers key (* (translate-val registers key) (translate-val registers val))))

(defn rcv [registers [val]]
  (if (not= 0 (translate-val registers val)) (translate-val registers "last-played")))

(defn snd [registers [val]]
  (d18set registers ["last-played" (translate-val registers val)]))

(defn jgz [registers [key val]]
  (let [x (translate-val registers key) y (translate-val registers val) result (if (> x 0) y 0)]
    (if (= 0 result) 1 result)))

(defn update-register [regs prog vals]
  (if (= prog "rcv") regs ((resolve (symbol prog)) regs vals)))

(defn run-prog [input]
  (loop [registers {"last-played" 0} i 0]
    (let [[prog vals] [(first (nth input i)) (drop 1 (nth input i))]]
      (if (and (= prog "rcv") (rcv registers vals))
        (rcv registers vals)
        (let [[new-reg new-i] (if (= prog "jgz")
                                [registers (+ i (jgz registers vals))]
                                [(update-register registers prog vals) (inc i)])]
          (recur new-reg new-i))
        ))))

(println "Part A:" (run-prog (read-input "../../resources/day_18_input.txt")))

(defn snd [registers [queue val]]
  (apply vector (concat [(translate-val registers val)] queue)))

(defn rcv [registers [queue val]]
  (d18set registers [val (peek queue)]))

(defn perform [input i reg own-q other-q send-count]
  (let [[prog vals] [(first (nth input i)) (drop 1 (nth input i))]]
    (cond
      (and (= prog "rcv") (= 0 (count other-q))) [i reg own-q other-q send-count]
      (= prog "rcv") [(inc i) (rcv reg [other-q (first vals)]) own-q (pop other-q) send-count]
      (= prog "snd") [(inc i) reg (snd reg [own-q (first vals)]) other-q (inc send-count)]
      (= prog "jgz") [(+ i (jgz reg vals)) reg own-q other-q send-count]
      :else [(inc i) (update-register reg prog vals) own-q other-q send-count])))

(defn run-part2 [input]
  (loop [[r1 r2] [{"p" 0} {"p" 1}] [q1 q2] [[] []] send-count [0 0] i1 0 i2 0]
    (let [[prog1 vals1] [(first (nth input i1)) (drop 1 (nth input i1))]
          [prog2 vals2] [(first (nth input i2)) (drop 1 (nth input i2))]]
      (if (and (= (count q1) 0) (= (count q2) 0) (= prog1 prog2) (= prog1 "rcv"))
        send-count
        (let [[i1 r1 q1 q2 sc1] (perform input i1 r1 q1 q2 (first send-count))
              [i2 r2 q2 q1 sc2] (perform input i2 r2 q2 q1 (second send-count))]
          (recur [r1 r2] [q1 q2] [sc1 sc2] i1 i2))))))

(println "Part B:" (second (run-part2 (read-input "../../resources/day_18_input.txt"))))