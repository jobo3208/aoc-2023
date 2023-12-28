(ns aoc-2023.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn h-a-s-h [s]
  (reduce
    (fn [v ch]
      (let [code (int ch)
            v (+ v code)
            v (* v 17)
            v (mod v 256)]
        v))
    0
    s))

(defn run-part-1 [input]
  (->> (string/split (string/trim input) #",")
       (map h-a-s-h)
       (reduce +)))

; To do Part 2 in the most efficient way, it sounds like an ordered map is
; needed.

; I looked at the implementation of
; <https://github.com/clj-commons/ordered/blob/master/src/flatland/ordered/map.clj>
; to get an idea of how one might do this.

; My understanding of their approach:
; We maintain two structures: a map and a vector.
; Each entry in the map maps the key to both the value and the index of the key
; in the vector.
; Each entry in the vector includes the key and the value.
; To insert, we insert the kv pair into the vector and note its index in the
; map.
; To delete, we dissoc the key from the map and set the index of the vector to
; nil.
; To get a sequence of keys/vals/kvs, we simply return the non-nil values of
; the vector.
; Ultimately, the price we pay to keep our map ordered is a) maintaining two
; structures instead of one, and b) keeping potentially many nil values in the
; vector if we do a lot of deletes.

(defn parse-op [s]
  (let [[_ label op focal-length] (re-matches #"^([a-z]+)([=-])(\d+)?$" s)]
    (if (= op "-")
      {:type \- :label label}
      {:type \= :label label :focal-length (Integer. focal-length)})))

(defmulti process-op (fn [_ op] (:type op)))

(defmethod process-op \- [boxes {:keys [label]}]
  (let [bi (h-a-s-h label)
        [bm bv] (nth boxes bi)]
    (if-let [li (get bm label)]
      (assoc boxes bi [(dissoc bm label) (assoc bv li nil)])
      boxes)))

(defmethod process-op \= [boxes {:keys [label focal-length]}]
  (let [bi (h-a-s-h label)
        [bm bv] (nth boxes bi)]
    (if-let [li (get bm label)]
      (assoc boxes bi [bm (assoc bv li [label focal-length])])
      (assoc boxes bi [(assoc bm label (count bv)) (conj bv [label focal-length])]))))

(defn focusing-power-in-box [bi [_ bv]]
  (->> bv
       (keep identity)
       (map second)
       (map-indexed (fn [li focal-length] (* (inc bi) (inc li) focal-length)))
       (reduce +)))

(defn run-part-2 [input]
  (let [boxes (into [] (repeat 256 [{} []]))
        ops (->> (string/split (string/trim input) #",")
                 (map parse-op))
        boxes' (reduce process-op boxes ops)]
    (->> (map-indexed focusing-power-in-box boxes')
         (reduce +))))

(comment
  (run-part-1 (slurp (io/resource "day_15.txt")))
  (run-part-2 (slurp (io/resource "day_15.txt"))))
