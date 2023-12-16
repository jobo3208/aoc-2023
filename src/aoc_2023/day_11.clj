(ns aoc-2023.day-11
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as s]
            [clojure.string :as string]))

(def sample-input "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn parse-line [line]
  (->> line
       (map-indexed vector)
       (filter #(= (second %) \#))
       (map first)))

(defn parse-image [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (map-indexed (fn [y xs] (mapv #(-> [y %]) xs)))
       (apply concat)
       (into [])))

(defn expand
  ([factor image]
   (->> image (expand factor 0) (expand factor 1)))
  ([factor n image]
   (let [occupied (set (map #(nth % n) image))
         all (set (range (apply min occupied) (apply max occupied)))
         unoccupied (s/difference all occupied)
         bump (fn [galaxy]
                (let [amount (count (filter #(> (nth galaxy n) %) unoccupied))]
                  (update galaxy n #(+ % (* (dec factor) amount)))))]
     (mapv bump image))))

(defn manhattan-distance [p q]
  (reduce + (map (comp abs -) p q)))

(defn run [factor input]
  (let [image (parse-image input)
        image' (expand factor image)]
    (->> (combo/combinations image' 2)
         (map (partial apply manhattan-distance))
         (reduce +))))

(comment
  (run 2 (slurp (io/resource "day_11.txt")))
  (run 1000000 (slurp (io/resource "day_11.txt"))))
