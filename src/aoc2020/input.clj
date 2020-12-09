(ns aoc2020.input
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-input
  ([filename]
   (read-input filename #"\n"))
  ([filename delimiter]
   (-> (io/resource filename)
       (slurp)
       (s/split delimiter))))

(defn strings->numbers [list]
  (map #(Integer/parseInt %) list))