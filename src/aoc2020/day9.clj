(ns aoc2020.day9
  (:require [aoc2020.input :as inp]
            [clojure.math.combinatorics :as c]))

(defn get-input [inploc]
  (->> (inp/read-input inploc)
       (inp/strings->numbers)))

(defn allowed-vals [initial]
  (->> (c/combinations initial 2)
       (map #(apply + %))
       set))

(defn validate-cipher [preamble-size numbers]
  (loop [preamble (vec (take preamble-size numbers))
         numbers' (drop preamble-size numbers)
         current (first numbers')
         next (rest numbers')
         index preamble-size]
    (if (clojure.set/subset? #{current} (allowed-vals preamble))
      (recur (conj (vec (drop 1 preamble)) (first numbers'))
             (drop 1 numbers')
             (first next)
             (rest next)
             (inc index))
      {:invalid current :index index :numbers numbers})))

(defn find-set [invalid-num numbers]
  (loop [cset []
         current (first numbers)
         next (rest numbers)]
    (let [cset' (conj cset current)]
      (cond (>= current invalid-num)        (recur [] (first next) (rest next))
            (= invalid-num (apply + cset')) cset'
            (> (apply + cset') invalid-num) (recur (vec (drop 1 cset)) current next)
            :else                           (recur cset' (first next) (rest next))))))

(comment
  (->> (get-input "day9.txt")
       (validate-cipher 25)) ; Part 1

  (->> (get-input "day9.txt")
       (validate-cipher 25)
       (#(find-set (:invalid %) (:numbers %)))
       (#(+ (apply min %) (apply max %)))) ; Part 2

  (max)

  (> 8 9))

