(ns aoc-2023.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "Time:      7  15   30
Distance:  9  40  200")

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (map #(BigInteger. %))))

(defn parse-races [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (apply map vector)))

(defn distance [hold-time race-time]
  (* hold-time (- race-time hold-time)))

(t/deftest test-distance
  (t/is (= (distance 0 7) 0))
  (t/is (= (distance 1 7) 6))
  (t/is (= (distance 2 7) 10))
  (t/is (= (distance 3 7) 12))
  (t/is (= (distance 4 7) 12))
  (t/is (= (distance 5 7) 10))
  (t/is (= (distance 7 7) 0)))

(defn num-winning-strategies [race]
  (let [[race-time record-dist] race]
    (->> (map #(distance % race-time) (range (inc race-time)))
         (filter (partial < record-dist))
         (count))))

(defn run-part-1 [input]
  (let [races (parse-races input)]
    (->> (map num-winning-strategies races)
         (reduce *))))

; It's easy to see from the small sample that the distance peaks at race-time / 2.
; So we can just start there and stop once the number falls below record-dist.
; Then we just double and adjust depending on even/odd race-time.

(defn num-winning-strategies-optimized [race]
  (let [[race-time record-dist] race
        n (->> (map #(distance % race-time) (range (quot race-time 2) (inc race-time)))
               (take-while (partial < record-dist))
               (count))]
    (if (even? race-time)
      (- (* 2 n) 1)
      (- (* 2 n) 2))))

(defn run-part-2 [input]
  (let [input (string/replace input #" +" "")
        races (parse-races input)]
    (->> (map num-winning-strategies-optimized races)
         (reduce *))))

(comment
  (run-part-1 (slurp (io/resource "day_6.txt")))
  (run-part-2 (slurp (io/resource "day_6.txt"))))
