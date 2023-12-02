(ns aoc-2023.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as t]))

(def sample-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn parse-color-showing [color-showing-str]
  (let [[number color] (string/split color-showing-str #" ")]
    {(keyword color) (Integer. number)}))

(defn parse-showing [showing-str]
  (let [init {:red 0 :green 0 :blue 0}
        color-showing-strs (string/split showing-str #", ")
        showings (map parse-color-showing color-showing-strs)]
    (reduce merge init showings)))

(t/deftest test-parse-showing
  (t/is (= (parse-showing "3 blue, 4 red") {:red 4 :green 0 :blue 3}))
  (t/is (= (parse-showing "2 green") {:red 0 :green 2 :blue 0}))
  (t/is (= (parse-showing "8 green, 6 blue, 20 red") {:red 20 :green 8 :blue 6})))

(defn parse-game [game-string]
  (let [[name desc] (string/split game-string #": ")
        id (Integer. (last (string/split name #" ")))
        showing-strs (string/split desc #"; ")
        showings (mapv parse-showing showing-strs)]
    {:id id :showings showings}))

(defn showing-possible? [bag showing]
  (every? true? (vals (merge-with >= bag showing))))

(t/deftest test-showing-possible?
  (t/is (true? (showing-possible? {:red 1 :green 1 :blue 1} {:red 0 :green 0 :blue 0})))
  (t/is (true? (showing-possible? {:red 1 :green 1 :blue 1} {:red 1 :green 0 :blue 1})))
  (t/is (false? (showing-possible? {:red 1 :green 1 :blue 1} {:red 2 :green 0 :blue 0}))))

(defn game-possible? [bag game]
  (every? (partial showing-possible? bag) (:showings game)))

(defn run-part-1 [input bag]
  (let [games (->> (string/split input #"\n") (map parse-game))]
    (->> games
         (filter (partial game-possible? bag))
         (map :id)
         (reduce +))))

(defn smallest-possible-bag [showings]
  (reduce (partial merge-with max) showings))

(t/deftest test-smallest-possible-bag
  (t/is (= (smallest-possible-bag [{:red 4 :green 0 :blue 3}
                                   {:red 1 :green 2 :blue 6}
                                   {:red 0 :green 2 :blue 0}])
           {:red 4 :green 2 :blue 6})))

(defn power [cube-set]
  (apply * (vals cube-set)))

(defn run-part-2 [input]
  (let [games (->> (string/split input #"\n") (map parse-game))]
    (->> games
         (map :showings)
         (map smallest-possible-bag)
         (map power)
         (reduce +))))

(comment
  (run-part-1 (slurp (io/resource "day_2.txt")) {:red 12 :green 13 :blue 14})
  (run-part-2 (slurp (io/resource "day_2.txt"))))
