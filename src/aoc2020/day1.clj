(ns aoc2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-input [filename]
  (->> (io/resource filename)
    (slurp)
    (s/split-lines)
    (map #(read-string %))))

(defn sum=2020? [a b]
  (= 2020 (+ a b)))

(defn compare-with-vector [number, numbers]
  (loop [current (first numbers)
         remaining (rest numbers)]
    (if (nil? current) current
                       (if (sum=2020? number current) current
                                                      (recur (first remaining) (rest remaining))))))

(defn find-2020 [numbers]
  (loop [current (first numbers)
         remaining (rest numbers)]
    (if (nil? current) current
                       (if-let [is2020 (compare-with-vector current numbers)]
                         (vector current is2020)
                         (recur (first remaining) (rest remaining))))))

(comment
  (read-input "day1.txt")
  (sum=2020? 1010 1011)
  (find-2020 (read-input "day1.txt"))
  (compare-with-vector 1024 (read-input "day1.txt"))
  (apply * (find-2020 (read-input "day1.txt"))))