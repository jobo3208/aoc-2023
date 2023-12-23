(ns aoc-2023.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn transpose [m]
  (apply mapv vector m))

(defn parse-pattern [pattern]
  (vec (string/split pattern #"\n")))

(defn halves-at [pattern i]
  (let [size (min i (- (count pattern) i))
        [left right] (split-at i pattern)
        left (take-last size left)
        right (take size right)]
    [left right]))

(defn find-horizontal-reflection [pattern]
  (->> (range 1 (count pattern))
       (filter (fn [i]
                 (let [[left right] (halves-at pattern i)]
                   (= left (reverse right)))))
       first))

(defn find-reflection [pattern]
  (if-let [y (find-horizontal-reflection pattern)]
    [:row y]
    [:col (find-horizontal-reflection (transpose pattern))]))

(defn summary [[axis i]]
  (if (= axis :row) (* 100 i) i))

(defn run-part-1 [input]
  (->> (string/split input #"\n\n")
       (map parse-pattern)
       (map find-reflection)
       (map summary)
       (reduce +)))

(defn diff [a b]
  (->> (map #(if (= %1 %2) 0 1) (apply str a) (apply str b))
       (reduce +)))

(defn find-smudged-horizontal-reflection [pattern]
  (->> (range 1 (count pattern))
       (filter (fn [i]
                 (let [[left right] (halves-at pattern i)]
                   (= (diff left (reverse right)) 1))))
       first))

(defn run-part-2 [input]
  (with-redefs [find-horizontal-reflection find-smudged-horizontal-reflection]
    (run-part-1 input)))

(comment
  (run-part-1 (slurp (io/resource "day_13.txt")))
  (run-part-2 (slurp (io/resource "day_13.txt"))))
