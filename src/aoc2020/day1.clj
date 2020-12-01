(ns aoc2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as c]))

(defn read-input [filename]
  (->> (io/resource filename)
    (slurp)
    (s/split-lines)
    (map #(read-string %))))

(defn sum=2020? [nums]
  (= 2020 (apply + nums)))

(defn find-2020 [combinations]
  (->> combinations
       (filter #(sum=2020? %))))

(comment
  (read-input "day1.txt")
  (as-> (read-input "day1.txt") nums
      (c/combinations nums 3)
      (find-2020 nums)
      (first nums)
      (apply * nums)))



