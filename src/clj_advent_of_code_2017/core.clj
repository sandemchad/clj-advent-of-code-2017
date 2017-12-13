(ns clj-advent-of-code-2017.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (combo/combinations [1 2 3 4 5]))
  (println "Hello, World!"))
