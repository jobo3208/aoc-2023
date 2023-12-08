(ns aoc-2023.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn translate [m v]
  (if-some [[d s r]
            (first (filter (fn [[d s r]]
                             (<= s v (+ s (dec r)))) m))]
    (+ d (- v s))
    v))

(t/deftest test-translate
  (let [m [[50 98 2] [52 50 48]]]
    (t/is (= (translate m 14) 14))
    (t/is (= (translate m 50) 52))
    (t/is (= (translate m 79) 81))
    (t/is (= (translate m 98) 50))
    (t/is (= (translate m 99) 51))))

(defn parse-num-list [s]
  (mapv #(BigInteger. %) (string/split s #" ")))

(defn parse-map [s]
  (mapv parse-num-list (rest (string/split s #"\n"))))

(defn traverse-maps [ms v]
  (reduce #(translate %2 %1) v ms))

(defn run-part-1 [input]
  (let [[seeds-str & maps-str] (string/split input #"\n\n")
        seeds (-> (string/split seeds-str #": ") second parse-num-list)
        maps (mapv parse-map maps-str)
        locations (map (partial traverse-maps maps) seeds)]
    (apply min locations)))

(defn run-part-2-naive [input]
  (let [[seeds-str & maps-str] (string/split input #"\n\n")
        seeds (-> (string/split seeds-str #": ") second parse-num-list)
        ; this naive implementation works fine on the sample, but will never
        ; work on the actual input
        seeds (->> (partition 2 seeds)
                   (map (fn [[s r]] (range s (+ s r))))
                   (apply concat))
        maps (mapv parse-map maps-str)
        locations (map (partial traverse-maps maps) seeds)]
    (apply min locations)))

; When stepping from one seed to the next as you go through a range, you will
; almost always end up one location higher. There are only a tiny number of
; "switching points" where that is not the case.

; The lowest location must come at either the beginning of a range, or one of
; these switching points.

; Each seed corresponds to a particular range within each map (including the
; implicit "identity" range). A switching point is any moment that the ranges
; change between two consecutive seeds. Can we identify the switching points?

; Since the beginnings of seed ranges count as switching points, start there.

; Then, we have to translate "backwards" through the maps.

; Look at seed-to-soil. 98 and 50 are already seeds, so they don't need any
; translation. They are switching points.

; Look at soil-to-fertilizer. 15, 52, and 0 are soil values, and are switching
; points. Their seed values can be gotten by simply swapping source and dest in
; the seed-to-soil map (making it soil-to-seed) and calling translate. Their
; seed values are 15, 50, and 0.

; Look at fertilizer-to-water.
; (This is probably the last one we have to step through manually.)
; The switching points for fertilizer are 53, 11, 0, and 7.
; The soil value for 53 is 14, and the seed value for 14 is 14.
; The soil value for 11 is 26, and the seed value for 26 is 26.
; The soil value for 0 is 15, and the seed value for 15 is 15.
; The soil value for 7 is 22, and the seed value for 22 is 22.

(defn reverse-map [m]
  (mapv (fn [[d s r]] [s d r]) m))

(defn extract-switching-points
  ; safe since the number of maps is always quite small
  ([ms]
   (extract-switching-points ms #{}))
  ([[m & ms] acc]
   (let [vs (map first m)]
     (if (empty? ms)
       (into acc vs)
       (let [sps (map #(traverse-maps ms %) vs)
             acc (into acc sps)]
         (extract-switching-points ms acc))))))

(defn in-range? [v [start end]]
  (<= start v (dec end)))

(defn in-any-range? [ranges v]
  (some (partial in-range? v) ranges))

(defn run-part-2 [input]
  (let [[seeds-str & maps-str] (string/split input #"\n\n")
        maps (mapv parse-map maps-str)
        maps-reversed (into [] (reverse (map (comp reverse-map parse-map) maps-str)))
        seeds (-> (string/split seeds-str #": ") second parse-num-list)
        seeds (->> (partition 2 seeds)
                   (map (fn [[s r]] [s (+ s r)])))
        switching-points (into #{} (apply concat seeds))
        switching-points (extract-switching-points maps-reversed switching-points)
        switching-points (filter (partial in-any-range? seeds) switching-points)
        locations (map (partial traverse-maps maps) switching-points)]
    (apply min locations)))

(comment
  (run-part-1 (slurp (io/resource "day_5.txt")))
  (run-part-2-naive sample-input)
  (run-part-2 (slurp (io/resource "day_5.txt"))))
