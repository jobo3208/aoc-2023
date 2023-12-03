(ns aoc-2023.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn re-seq-m
  "Like re-seq, but returns original match objects. Copied from core
  lib and adjusted. Not sure there's a better way to do this."
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
       (when (. m (find))
         (cons m (lazy-seq (step))))))))

(defn parse-number [m]
  {:type :number
   :start (.start m)
   :end (dec (.end m))
   :value (Integer. (re-groups m))})

(defn parse-symbol [m]
  {:type :symbol
   :start (.start m)
   :end (dec (.end m))
   :value (first (re-groups m))})

(defn parse-line [line]
  (let [numbers (->> (re-seq-m #"\d+" line) (map parse-number))
        symbols (->> (re-seq-m #"[^\.\d]" line) (map parse-symbol))]
    (->> (concat numbers symbols)
         (sort-by :start)
         (into []))))

(t/deftest test-parse-line
  (t/is (= (parse-line "467..114..") [{:type :number :start 0 :end 2 :value 467}
                                      {:type :number :start 5 :end 7 :value 114}]))
  (t/is (= (parse-line "...*......") [{:type :symbol :start 3 :end 3 :value \*}]))
  (t/is (= (parse-line "617*......") [{:type :number :start 0 :end 2 :value 617}
                                      {:type :symbol :start 3 :end 3 :value \*}])))

(defn parse-schematic [input]
  (let [lines (string/split input #"\n")]
    (->> (map parse-line lines)
         (map-indexed (fn [i row]
                        (mapv #(assoc % :row i) row)))
         (into []))))

(defn adjacent-row-wise? [a b]
  (<= (abs (- (:row a) (:row b))) 1))

(defn adjacent-col-wise? [a b]
  (let [max-start (max (:start a) (:start b))
        min-end (min (:end a) (:end b))]
    (<= max-start (inc min-end))))

(defn adjacent? [a b]
  (and (adjacent-row-wise? a b)
       (adjacent-col-wise? a b)))

(t/deftest test-adjacent?
  (t/is (true? (adjacent? {:type :number, :start 0, :end 2, :value 467, :row 0}
                          {:type :symbol, :start 3, :end 3, :value \*, :row 1})))
  (t/is (true? (adjacent? {:type :number, :start 0, :end 2, :value 617, :row 4}
                          {:type :symbol, :start 3, :end 3, :value \*, :row 4})))
  (t/is (false? (adjacent? {:type :symbol, :start 5, :end 5, :value \+, :row 5}
                           {:type :number, :start 7, :end 8, :value 58, :row 5}))))

(defn part? [number symbols]
  (boolean (some #(adjacent? number %) symbols)))

(defn find-parts-in-row [pre row post]
  (let [numbers (filter #(= (:type %) :number) row)
        symbols (filter #(= (:type %) :symbol) (concat pre row post))]
    (filter #(part? % symbols) numbers)))

(defn run-part-1 [input]
  (let [schematic (parse-schematic input)
        schematic (into [] (concat [[]] schematic [[]])) ; pad for partition
        parts (reduce concat (map #(apply find-parts-in-row %) (partition 3 1 schematic)))]
    (->> parts
         (map :value)
         (reduce +))))

(defn neighbors [a bs]
  (filter #(adjacent? a %) bs))

(defn find-gears-in-row [pre row post]
  (let [possible-gears (filter #(= (:value %) \*) row)
        numbers (filter #(= (:type %) :number) (concat pre row post))]
    (->> possible-gears
         (map (juxt identity #(neighbors % numbers)))
         (filter (fn [[_ nbrs]] (= (count nbrs) 2)))
         (map (fn [[gear [n1 n2]]]
                (assoc gear :ratio (* (:value n1) (:value n2))))))))

(defn run-part-2 [input]
  (let [schematic (parse-schematic input)
        schematic (into [] (concat [[]] schematic [[]])) ; pad for partition
        gears (reduce concat (map #(apply find-gears-in-row %) (partition 3 1 schematic)))]
    (->> gears
         (map :ratio)
         (reduce +))))

(comment
  (run-part-1 (slurp (io/resource "day_3.txt")))
  (run-part-2 (slurp (io/resource "day_3.txt"))))
