(ns aoc-2023.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn move [[y x] dir]
  (case dir
    :north [(dec y) x]
    :west  [y (dec x)]
    :south [(inc y) x]
    :east  [y (inc x)]))

(defmulti advance-beam
  "Advances the beam through the tile, returning one or more new beams in a vector."
  (fn [_ tile] tile))

(defmethod advance-beam \. [[[y x] dir] _]
  [[(move [y x] dir) dir]])

(defmethod advance-beam \\ [[[y x] dir] _]
  (let [dir' (case dir
               :north :west
               :west  :north
               :south :east
               :east  :south)]
    [[(move [y x] dir') dir']]))

(defmethod advance-beam \/ [[[y x] dir] _]
  (let [dir' (case dir
               :north :east
               :west  :south
               :south :west
               :east  :north)]
    [[(move [y x] dir') dir']]))

(defmethod advance-beam \- [[[y x] dir :as beam] _]
  (if (#{:west :east} dir)
    (advance-beam beam \.)
    [[(move [y x] :west) :west] [(move [y x] :east) :east]]))

(defmethod advance-beam \| [[[y x] dir :as beam] _]
  (if (#{:north :south} dir)
    (advance-beam beam \.)
    [[(move [y x] :north) :north] [(move [y x] :south) :south]]))

(defn parse-map [input]
  (->> (string/split input #"\n")
       (mapv vec)))

(defn out-of-bounds? [h w [y x]]
  (or (neg? y)
      (neg? x)
      (>= y h)
      (>= x w)))

(defn trace-beam [m beam]
  (let [h (count m)
        w (count (first m))]
    (loop [frontier [beam]
           history #{}]
      (if (empty? frontier)
        (into #{} (map first history))
        (let [frontier' (mapcat #(advance-beam % (get-in m (first %))) frontier)
              frontier' (remove #(out-of-bounds? h w (first %)) frontier')
              frontier' (remove history frontier')
              history' (into history frontier)]
          (recur frontier' history'))))))

(defn run-part-1 [input]
  (let [m (parse-map input)]
    (count (trace-beam m [[0 0] :east]))))

; Brute forcing the different start beams won't be prohibitively slow in this
; case, so let's just do that.

(defn run-part-2 [input]
  (let [m (parse-map input)
        h (count m)
        w (count (first m))
        possible-beams
        (concat
          (map #(-> [[0 %] :south]) (range w))
          (map #(-> [[% 0] :east]) (range h))
          (map #(-> [[(dec h) %] :north]) (range w))
          (map #(-> [[% (dec w)] :west]) (range h)))]
    (apply max (map #(count (trace-beam m %)) possible-beams))))

(comment
  (run-part-1 (slurp (io/resource "day_16.txt")))
  (run-part-2 (slurp (io/resource "day_16.txt"))))
