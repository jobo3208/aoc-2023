(ns aoc-2023.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(def code->dir {\R :east \D :south \L :west \U :north})

(defn parse-step [line]
  (let [[_ dir distance color] (re-matches #"^(.) (\d+) \((#[0-9a-f]{6})\)$" line)]
    [(code->dir (first dir)) (Integer. distance) color]))

(defn move [[y x] dir]
  (case dir
    :north [(dec y) x]
    :west  [y (dec x)]
    :south [(inc y) x]
    :east  [y (inc x)]))

(defn trace-step [start [dir dist color]]
  (->> (iterate #(move % dir) start)
       (take (inc dist))
       (mapv #(-> {:pos % :dir dir :color color}))))

(defn trace-steps [start steps]
  (reduce
    (fn [path step]
      (let [start (or (:pos (last path)) start)]
        (into path (trace-step start step))))
    []
    steps))

(defn center-trace [trace]
  (let [min-y (apply min (map (comp first :pos) trace))
        min-x (apply min (map (comp second :pos) trace))]
    (mapv #(let [[y x] (:pos %)]
             (assoc % :pos [(- y min-y) (- x min-x)]))
          trace)))

(defn edge [trace-dir dir]
  (case [trace-dir dir]
    [:cw :north] :west
    [:cw :west]  :south
    [:cw :south] :east
    [:cw :east]  :north
    [:ccw :north] :east
    [:ccw :west]  :north
    [:ccw :south] :west
    [:ccw :east]  :south))

(def opp-dir
  {:east :west
   :west :east
   :north :south
   :south :north})

(defn establish-edges [trace]
  (let [start (->> trace
                   (filter (comp #{:east :west} :dir))
                   (sort-by :pos)
                   first)
        trace-dir (case (:dir start) :east :cw :west :ccw)
        ; rearrange trace so that it begins at start
        [before after] (split-with (partial not= start) trace)
        trace (concat after before)]
    (map #(assoc % :edge (edge trace-dir (:dir %))) trace)))

(defn get-area [trace]
  (let [outline (into #{} (map :pos trace))]
    (reduce
      (fn [area step]
        (let [facing (opp-dir (:edge step))
              los (drop 1 (iterate #(move % facing) (:pos step)))]
          (into area (take-while (complement outline) los))))
      outline
      trace)))

(defn run-part-1 [input]
  (let [steps (->> (string/split input #"\n")
                   (map parse-step))
        trace (trace-steps [0 0] steps)
        trace (center-trace trace)
        trace (establish-edges trace)
        area (get-area trace)]
    (count area)))

; This is one of those cases where my solution for part 1 is basically useless for part 2.

; It's clear now that we are going to have to use line segments instead of individual points.

(defn parse-step' [line]
  (let [[_ _ code] (parse-step line)
        dist (Integer/parseInt (subs code 1 6) 16)
        dir (case (last code) \0 :east \1 :south \2 :west \3 :north)]
    [dir dist]))

(defn move-by [[y x] dir n]
  (case dir
    :north [(- y n) x]
    :west  [y (- x n)]
    :south [(+ y n) x]
    :east  [y (+ x n)]))

(defn trace-steps' [start steps]
  (reduce
    (fn [path [dir dist]]
      (let [start (or (-> path last :segment second) start)]
        (conj path {:dir dir
                    :segment [start (move-by start dir dist)]})))
    []
    steps))

(defn get-trace-dir [trace]
  (let [topmost (->> trace
                     (filter (comp #{:east :west} :dir))
                     (sort-by (comp ffirst :segment))
                     first)]
    (case (:dir topmost) :east :cw :west :ccw)))

(defn establish-edges' [trace]
  (let [trace-dir (get-trace-dir trace)]
    (mapv #(assoc % :edge (edge trace-dir (:dir %))) trace)))

(defn get-rects [trace]
  (let [ys (->> (mapcat (comp (partial map first) :segment) trace)
                (into #{})
                (sort))
        xs (->> (mapcat (comp (partial map second) :segment) trace)
                (into #{})
                (sort))]
    (for [[y1 y2] (partition 2 1 ys)
          [x1 x2] (partition 2 1 xs)]
      [[y1 x1] [y2 x2]])))

(defn directly-below? [rect horizontal]
  (let [ry2 (first (second rect))
        sy2 (first (sort (map first (:segment horizontal))))
        [rx1 rx2] (map second rect)
        [sx1 sx2] (sort (map second (:segment horizontal)))]
    (and (>= sy2 ry2)
         (>= rx1 sx1)
         (<= rx2 sx2))))

(defn contained? [rect horizontals]
  (let [closest-horizontal (first (filter (partial directly-below? rect) horizontals))]
    (boolean (= (:edge closest-horizontal) :south))))

(defn get-rect-area [rect]
  (let [[[y1 x1] [y2 x2]] rect]
    (* (abs (- y2 y1)) (abs (- x2 x1)))))

(defn get-area' [trace]
  (let [rects (get-rects trace)
        horizontals (->> trace
                         (filter (comp #{:east :west} :dir))
                         (sort-by (comp ffirst :segment)))]
    (reduce
      (fn [area rect]
        (if (contained? rect horizontals)
          (+ area (get-rect-area rect))
          area))
      0
      rects)))

(defn manhattan-distance [p q]
  (reduce + (map (comp abs -) p q)))

; get-border-area is the best way I can think of to adjust for the fact that
; all my calculations assume 0-width lines, where the problem actually uses
; 1-width lines. Instead of rethinking everything, I just came up with a way to
; add the extra area.

(defn get-border-area [trace]
  (let [perimeter (->> trace
                       (map #(apply manhattan-distance (:segment %)))
                       (reduce +))
        corners (->> (conj trace (first trace))
                     (partition 2 1)
                     (map (fn [[s1 s2]]
                            (let [s1-facing (-> s1 :edge opp-dir)]
                              (if (= s1-facing (:dir s2))
                                :outer :inner))))
                     (frequencies))]
    (+ (* perimeter 1/2)
       (* (:outer corners) 1/4)
       (- (* (:inner corners) 1/4)))))

(defn run-part-2 [input]
  (let [steps (->> (string/split input #"\n")
                   (map parse-step'))
        trace (trace-steps' [0 0] steps)
        trace (establish-edges' trace)]
    (+ (get-area' trace) (get-border-area trace))))

(comment
  (run-part-1 (slurp (io/resource "day_18.txt")))
  (run-part-2 (slurp (io/resource "day_18.txt"))))
