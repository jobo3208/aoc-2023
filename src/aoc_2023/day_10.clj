(ns aoc-2023.day-10
  (:require [clojure.java.io :as io]
            [clojure.set :as se]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def sample-input-2 "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(defn parse-input [input]
  (->> (string/split input #"\n")
       (mapv vec)))

(defn find-start [grid]
  (->> (for [y (range (count grid))
             x (range (count (first grid)))]
         [y x])
       (filter #(= \S (get-in grid %)))
       first))

(def pipe-deltas
  {\S [[-1 0] [0 1] [1 0] [0 -1]]
   \| [[-1 0] [1 0]]
   \- [[0 1] [0 -1]]
   \F [[0 1] [1 0]]
   \7 [[1 0] [0 -1]]
   \J [[-1 0] [0 -1]]
   \L [[-1 0] [0 1]]})

(defn connecting? [grid p1 p2]
  (let [[t1 t2] (map (partial get-in grid) [p1 p2])
        [ds1 ds2] (map pipe-deltas [t1 t2])]
    (->> (for [d1 ds1, d2 ds2]
           (let [p1' (map + p1 d1)
                 p2' (map + p2 d2)]
             (and (= p1 p2') (= p1' p2))))
         (some true?)
         boolean)))

(t/deftest test-connecting?
  (let [grid (parse-input sample-input)]
    (t/is (false? (connecting? grid [1 1] [1 1])))
    (t/is (false? (connecting? grid [1 1] [1 0])))
    (t/is (true? (connecting? grid [1 1] [1 2])))
    (t/is (false? (connecting? grid [1 1] [1 3])))
    (t/is (true? (connecting? grid [1 1] [2 1])))))

(defn neighbors [[y x]]
  [[(dec y) x]
   [y (dec x)]
   [(inc y) x]
   [y (inc x)]])

(defn find-loop-distances [grid]
  (let [start (find-start grid)]
    (loop [[p & ps] [start]
           distances {start 0}]
      (if-not p
        distances
        (let [connections (->> (neighbors p)
                               (filter (partial connecting? grid p)))
              new-ps (se/difference (set connections) (set (keys distances)))
              distances' (merge distances (zipmap new-ps (repeat (inc (distances p)))))]
          (recur (into (vec ps) new-ps) distances'))))))

(defn run-part-1 [input]
  (let [grid (parse-input input)
        distances (find-loop-distances grid)]
    (apply max (vals distances))))

; First, be sure to establish what piece S is functioning as.
; Find the leftmost of the topmost pieces of the loop.
; This piece must be either an F, or an S functioning as an F.
; This piece is guaranteed not to have any ground south or east of it, because those spaces must contain pieces of the loop.
; Start by moving east while looking south.
; At each point, in the direction you are looking, all tiles that appear before the next loop piece in your LOS are enclosed by the loop.
; When you encounter a corner piece, look, then turn (both your moving and looking directions) then look again. Then continue.
; Stop when you have returned to the point you started at.

(def sample-input-3 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def sample-input-4 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def sample-input-5 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(defn start-piece [grid]
  (let [start-point (find-start grid)
        connections (->> (neighbors start-point)
                         (filter (partial connecting? grid start-point)))
        deltas (set (map #(mapv - % start-point) connections))
        piece-lookup (zipmap (map set (vals pipe-deltas)) (keys pipe-deltas))]
    (piece-lookup deltas)))

(defn swap-start-piece [grid]
  (assoc-in grid (find-start grid) (start-piece grid)))

(defn points-in-direction [h w [y x] direction]
  (case direction
    :east (map #(-> [y %]) (range (inc x) w))
    :south (map #(-> [% x]) (range (inc y) h))
    :west (map #(-> [y %]) (range (dec x) -1 -1))
    :north (map #(-> [% x]) (range (dec y) -1 -1))))

(t/deftest test-points-in-direction
  (t/is (= (points-in-direction 3 3 [0 0] :east) '([0 1] [0 2])))
  (t/is (= (points-in-direction 3 3 [0 0] :south) '([1 0] [2 0])))
  (t/is (= (points-in-direction 3 3 [0 0] :west) '()))
  (t/is (= (points-in-direction 3 3 [0 0] :north) '()))
  (t/is (= (points-in-direction 3 3 [1 1] :north) '([0 1])))
  (t/is (= (points-in-direction 3 3 [1 1] :west) '([1 0])))
  (t/is (= (points-in-direction 3 3 [2 2] :east) '())))

(def moving->facing
  {:east :south
   :south :west
   :west :north
   :north :east})

(defn turn [piece direction]
  (case [piece direction]
    [\F :north] :east
    [\F :west] :south
    [\J :south] :west
    [\J :east] :north
    [\L :south] :east
    [\L :west] :north
    [\7 :east] :south
    [\7 :north] :west))

(defn find-enclosed [grid loop-ps]
  (let [start (first (sort loop-ps)) ; leftmost topmost
        pid (partial points-in-direction (count grid) (count (first grid)))]
    (loop [moving :east
           p (first (pid start moving))
           enclosed #{}
           turned false]
      (if (= p start)
        enclosed
        (let [facing (moving->facing moving)
              segment (take-while (complement loop-ps) (pid p facing))
              enclosed' (into enclosed segment)
              piece (get-in grid p)]
          (if (and (contains? #{\F \J \L \7} piece) (not turned))
            (recur (turn piece moving) p enclosed' true)
            (recur moving (first (pid p moving)) enclosed' false)))))))

(defn run-part-2 [input]
  (let [grid (parse-input input)
        loop-ps (set (keys (find-loop-distances grid)))
        grid (swap-start-piece grid)]
    (count (find-enclosed grid loop-ps))))

(comment
  (run-part-1 (slurp (io/resource "day_10.txt")))
  (run-part-2 (slurp (io/resource "day_10.txt"))))
