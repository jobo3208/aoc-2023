(ns aoc-2023.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def sample-input-part-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn extract-digits [s]
  (let [digits (into #{} "0123456789")]
    (apply str (filter digits s))))

(def word->digit
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn words-to-digits-naive [s]
  ; This isn't good enough -- it needs to replace in order from left to right
  ; so that "eighttwo" becomes "8wo", not "eight2".
  (reduce (fn [s' [word digit]]
            (string/replace s' word digit))
          s
          word->digit))

(defn words-to-digits
  ([s]
   (words-to-digits s false))
  ([s reverse?]
   (let [word->digit (if reverse? (update-keys word->digit string/reverse) word->digit)
         words (keys word->digit)
         pattern (re-pattern (string/join "|" words))]
     (string/replace s pattern word->digit))))

(defn run-part-1 [input]
  (->> (string/split input #"\n")
       (map extract-digits)
       (map #(str (first %) (last %)))
       (map #(Integer. %))
       (reduce +)))

(defn run-part-2 [input]
  (let [first-digit (comp first extract-digits words-to-digits)
        last-digit (comp first extract-digits #(words-to-digits % true) string/reverse)]
    (->> (string/split input #"\n")
         (map (juxt first-digit last-digit))
         (map (partial apply str))
         (map #(Integer. %))
         (reduce +))))

(comment
  (run-part-1 (slurp (io/resource "day_1.txt")))
  (run-part-2 (slurp (io/resource "day_1.txt"))))
