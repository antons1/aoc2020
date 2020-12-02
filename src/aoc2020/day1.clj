(ns aoc2020.day1
  (:require [clojure.math.combinatorics :as c]
            [aoc2020.input :refer [read-input strings->numbers]]))

(defn =2020? [nums]
  (= 2020 (apply + nums)))

(defn find-2020 [combinations]
  (->> combinations
       (filter #(=2020? %))))

(comment
  (read-input "day1.txt")
  (as-> (read-input "day1.txt") nums
        (strings->numbers nums)
      (c/combinations nums 3)
      (find-2020 nums)
      (first nums)
      (apply * nums)))



