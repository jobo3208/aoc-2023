(ns aoc-2023.day-4
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]))

(def sample-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn string->set [num-str]
  (->> (string/split (string/trim num-str) #" +")
       (map #(Integer. %))
       (into #{})))

(defn parse-card [line]
  (let [[_ winning-str have-str] (string/split line #"(: | \| )")]
    [(string->set winning-str) (string->set have-str)]))

(defn score-card [card]
  (let [[winning have] card
        winners (count (s/intersection winning have))]
    (if (pos? winners)
      (Math/pow 2 (dec winners))
      0)))

(defn run-part-1 [input]
  (->> (string/split input #"\n")
       (map parse-card)
       (map score-card)
       (reduce +)))

(defn num-matches [card]
  (let [[winning have] card]
    (count (s/intersection winning have))))

(defn run-part-2 [input]
  (let [values (->> (string/split input #"\n")
                    (map parse-card)
                    (mapv num-matches))
        ; card counts start at 1 for each
        init-counts (into [] (repeat (count values) 1))
        counts (reduce
                 (fn [cs [i v]]
                   ; increment the next v counts by c (once for each copy of
                   ; the current card)
                   (let [c (nth cs i)
                         inc-fn #(+ c %)
                         inc-range (range (inc i) (+ (inc i) v))]
                     (reduce #(update %1 %2 inc-fn) cs inc-range)))
                 init-counts
                 (map-indexed vector values))]
    (reduce + counts)))

(comment
  (run-part-1 (slurp (io/resource "day_4.txt")))
  (run-part-2 (slurp (io/resource "day_4.txt"))))
