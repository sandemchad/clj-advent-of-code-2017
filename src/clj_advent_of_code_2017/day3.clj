(ns clj-advent-of-code-2017.day3)

(def target 347991)

(defn empty-matrix [m n val]
  (apply vector (repeat m (apply vector (repeat n val)))))

(defn spiral-matrix [m n & [start]]
  (let [row (vector (map #(- start %) (range m)))]
    (if (= 1 n) row
                (concat row (mapv reverse
                                 (apply map vector
                                        (spiral-matrix (dec n) m (- start m))))))))

(defn neighbors [[i j]]
  (let [x ((juxt inc inc identity dec dec dec identity inc) i)
        y ((juxt identity inc inc inc identity dec dec dec) j)]
    (map vector x y)))

(defn sum-neighbors [m coord]
  (reduce + (filter identity (map #(get-in m %) (neighbors coord)))))

(defn add-spiral [m coord]
  (let [val (sum-neighbors m coord)]
    (let [new-matrix (assoc-in m coord (if (= 0 val) 1 val))]
      (if (> val target) (reduced new-matrix) new-matrix))))

(defn find-index [e list]
  (first
    (for [[x row] (map-indexed vector list)
         [y val] (map-indexed vector row)
         :when (= e val)]
     [x y])))

(defn abs [n] (max n (- n)))

(defn pow-two [n]
  (int (* n n)))

(defn next-int-sqrt [n]
  (let [s (Math/sqrt n)]
    (if (= 0 (mod s 1))
      n
      (pow-two (int (+ 1 s)))))
  )

(def countdown (next-int-sqrt target))

(let [len (int (Math/sqrt countdown))]
  (def spiral (spiral-matrix len len countdown))
  (def new-spiral (empty-matrix len len 0))
  (let [[sx sy] (find-index 1 spiral) [tx ty] (find-index target spiral)]
    (println "Part A:" (+ (abs (- sx tx)) (abs (- sy ty)))))
  (let [update-seq (map #(find-index % spiral) (range 1 (+ 1 countdown)))]
    (def complete-spiral (reduce add-spiral new-spiral update-seq))
    (println "Part B:" (last (sort (flatten complete-spiral)))))
  )

