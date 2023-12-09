(ns aoc-2023.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defn card-strength [card]
  (string/index-of "23456789TJQKA" card))

(defn hand-type-strength [hand-type]
  (get (zipmap [:high-card :pair :two-pair :three :full-house :four :five] (range)) hand-type))

(defn hand-type [hand]
  (let [freqs (->> hand frequencies vals frequencies)]
    (cond
      (freqs 5) :five
      (freqs 4) :four
      (and (freqs 3) (freqs 2)) :full-house
      (freqs 3) :three
      (= (freqs 2) 2) :two-pair
      (= (freqs 2) 1) :pair
      :else :high-card)))

(t/deftest test-hand-type
  (t/is (= (hand-type "4K362") :high-card))
  (t/is (= (hand-type "32T3K") :pair))
  (t/is (= (hand-type "KK677") :two-pair))
  (t/is (= (hand-type "QQQJA") :three))
  (t/is (= (hand-type "77333") :full-house))
  (t/is (= (hand-type "22228") :four))
  (t/is (= (hand-type "99999") :five)))

(defn hand-strength [hand]
  [(-> hand hand-type hand-type-strength)
   (mapv card-strength hand)])

(defn parse-line [line]
  (let [[hand bid-str] (string/split line #" ")]
    [hand (Integer. bid-str)]))

(defn run-part-1 [input]
  (->> (string/split input #"\n")
       (map parse-line)
       (sort-by (comp hand-strength first))
       (map-indexed (fn [i [hand bid]] (* (inc i) bid)))
       (reduce +)))

(defn card-strength' [card]
  (string/index-of "J23456789TQKA" card))

(defn hand-type' [hand]
  (let [num-jokers (count (filter (partial = \J) hand))
        ; add jokers to the best thing you've got
        freqs (->> hand (remove (partial = \J)) frequencies vals sort reverse (into []))
        freqs (if (empty? freqs) [0] freqs)
        freqs (update freqs 0 #(+ % num-jokers))
        freqs (frequencies freqs)]
    (cond
      (freqs 5) :five
      (freqs 4) :four
      (and (freqs 3) (freqs 2)) :full-house
      (freqs 3) :three
      (= (freqs 2) 2) :two-pair
      (= (freqs 2) 1) :pair
      :else :high-card)))

(t/deftest test-hand-type'
  (t/is (= (hand-type' "32T3K") :pair))
  (t/is (= (hand-type' "KK677") :two-pair))
  (t/is (= (hand-type' "T55J5") :four))
  (t/is (= (hand-type' "QQQJA") :four))
  (t/is (= (hand-type' "KTJJT") :four))
  (t/is (= (hand-type' "JJJJJ") :five)))

(defn run-part-2 [input]
  (with-redefs [card-strength card-strength'
                hand-type hand-type']
    (run-part-1 input)))

(comment
  (run-part-1 (slurp (io/resource "day_7.txt")))
  (run-part-2 (slurp (io/resource "day_7.txt"))))
