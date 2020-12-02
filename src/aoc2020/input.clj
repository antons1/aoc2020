(ns aoc2020.input
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-input [filename]
  (->> (io/resource filename)
       (slurp)
       (s/split-lines)))

(defn strings->numbers [list]
  (map #(read-string %) list))