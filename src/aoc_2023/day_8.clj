(ns aoc-2023.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *print-length* 6)

(def sample-input "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn parse-network-line [line]
  (let [[_ k vl vr] (re-find #"(...) = \((...), (...)\)" line)]
    [k [vl vr]]))

(defn parse-input [input]
  (let [lines (string/split input #"\n")
        instructions (first lines)
        network (->> lines
                     (drop 2)
                     (map parse-network-line)
                     (into {}))]
    [instructions network]))

(defn next-loc [network loc instruction]
  (let [i-idx (if (= instruction \L) 0 1)]
    (get-in network [loc i-idx])))

(defn num-steps-to [network instructions start end]
  (loop [[i & is] (cycle instructions)
         loc start
         dist 0]
    (if (= loc end)
      dist
      (recur is (next-loc network loc i) (inc dist)))))

(defn run-part-1 [input]
  (let [[instructions network] (parse-input input)]
    (num-steps-to network instructions "AAA" "ZZZ")))

(def sample-input-2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn num-steps-to' [network instructions start-letter end-letter]
  (loop [[i & is] (cycle instructions)
         locs (->> network keys (filter #(= (last %) start-letter)) (into []))
         dist 0]
    (if (every? #(string/ends-with? % (str end-letter)) locs)
      dist
      (recur is (mapv #(next-loc network % i) locs) (inc dist)))))

(defn run-part-2-naive [input]
  (let [[instructions network] (parse-input input)]
    (num-steps-to' network instructions \A \Z)))

; Taking too long.

; My guess is that each ghost probably goes through a cycle, and it's a matter
; of figuring out the least common multiple.

(defn find-cycle-distance [network instructions start end-letter]
  (->> (reductions (partial next-loc network) start instructions)
       (map-indexed vector)
       (filter (comp #(string/ends-with? % (str end-letter)) second))
       ffirst))

(defn multiples [n]
  (map (partial * n) (drop 1 (range))))

(defn least-common-multiple-naive [nums]
  (loop [mults (mapv multiples nums)]
    (let [ms (mapv first mults)]
      (if (apply = ms)
        (first ms)
        (let [max-m (apply max ms)]
          (recur (mapv #(if (< (first %) max-m) (drop 1 %) %) mults)))))))

; My naive lcm was far too slow, so I got ChatGPT to show me the more efficient version.
; As it turns out, the version ChatGPT gave me did not produce the correct answer.
; I think there was some silent integer overflow going on somewhere.
; Below is the adjusted version that produces the correct answer.

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (quot (* a b) (gcd a b))))

(defn lcm-multiple [numbers]
  (reduce lcm numbers))

(defn run-part-2 [input]
  (let [[instructions network] (parse-input input)
        starts (->> network keys (filter #(= (last %) \A)) (into []))
        cycle-dists (map #(find-cycle-distance network (cycle instructions) % \Z) starts)]
    (lcm-multiple cycle-dists)))

(comment
  (run-part-1 (slurp (io/resource "day_8.txt")))
  (run-part-2 (slurp (io/resource "day_8.txt"))))
