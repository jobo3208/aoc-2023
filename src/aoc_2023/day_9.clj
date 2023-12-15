(ns aoc-2023.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn diffs [xs]
  (->> xs
       (partition 2 1)
       (mapv (fn [[a b]] (- b a)))))

(defn take-until [pred coll]
  (let [idx (->> coll
                 (map-indexed vector)
                 (drop-while (comp (complement pred) second))
                 ffirst)]
    (take (inc idx) coll)))

(defn extrapolate-forward [xs ys]
  (conj ys (+ (last xs) (last ys))))

(defn extrapolate [step history]
  (->> history
       (iterate diffs)
       (take-until (partial every? zero?))
       (reverse)
       (reduce step)))

(defn parse-line [line]
  (->> (string/split line #" ")
       (mapv #(Integer. %))))

(defn run-part-1 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (map (partial extrapolate extrapolate-forward))
       (map peek)
       (reduce +)))

(defn extrapolate-backward [xs ys]
  (cons (- (first ys) (first xs)) ys))

(defn run-part-2 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (map (partial extrapolate extrapolate-backward))
       (map first)
       (reduce +)))

(comment
  (run-part-1 (slurp (io/resource "day_9.txt")))
  (run-part-2 (slurp (io/resource "day_9.txt"))))
