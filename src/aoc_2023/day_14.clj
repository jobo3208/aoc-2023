(ns aoc-2023.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn parse-map [input]
  (->> (string/split input #"\n")
       (mapv vec)))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate-cw [m]
  (mapv (comp vec reverse) (transpose m)))

(defn rotate-ccw [m]
  (-> m rotate-cw rotate-cw rotate-cw))

(defn tilt-row [row]
  (->> (partition-by #{\#} row)
       (map (comp reverse sort))
       flatten
       (into [])))

(t/deftest test-tilt-row
  (t/is (= (tilt-row (vec "OO.O.O..##")) (vec "OOOO....##")))
  (t/is (= (tilt-row (vec "...OO....O")) (vec "OOO.......")))
  (t/is (= (tilt-row (vec ".O...#O..O")) (vec "O....#OO..")))
  (t/is (= (tilt-row (vec "#.#..O#.##")) (vec "#.#O..#.##"))))

(def turn-fn
  {:north rotate-ccw
   :west identity
   :south rotate-cw
   :east (comp rotate-cw rotate-cw)})

(def turn-back-fn
  {:north rotate-cw
   :west identity
   :south rotate-ccw
   :east (comp rotate-ccw rotate-ccw)})

(defn tilt [m dir]
  (let [turn (turn-fn dir)
        turn-back (turn-back-fn dir)]
    (->> m
         turn
         (map tilt-row)
         turn-back)))

(defn calculate-load [m]
  (->> m
       reverse
       (map-indexed (fn [i row]
                      (* (inc i) (count (filter #{\O} row)))))
       (reduce +)))

(defn run-part-1 [input]
  (let [m (parse-map input)
        m' (tilt m :north)]
    (calculate-load m')))

(defn find-cycle [xs]
  ; borrowed from aoc-2022, day 17
  (let [total-length (count xs)]
    (loop [part-length (quot total-length 2)
           start 0]
      (when (> part-length 1)
        (let [xs (drop start xs)
              sublists (partition part-length xs)]
          (if (and (> (count sublists) 2) (apply = sublists))
            [start part-length]
            (if (< start (- total-length (* 2 part-length)))
              (recur part-length (inc start))
              (recur (dec part-length) 0))))))))

(defn run-part-2 [input cycle-search-size]
  (let [m (parse-map input)
        loads (->> m
                   (iterate #(reduce tilt % [:north :west :south :east]))
                   (map calculate-load)
                   (take cycle-search-size))
        [start period] (find-cycle loads)
        i (mod (- 1000000000 start) period)]
    (nth loads (+ start i))))

(comment
  (run-part-1 (slurp (io/resource "day_14.txt")))
  (run-part-2 (slurp (io/resource "day_14.txt")) 200))
