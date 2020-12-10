(ns aoc2020.day10
  (:require [aoc2020.input :as inp]))

(defn input [inploc]
  (->> (inp/read-input inploc)
       (inp/strings->numbers)
       (#(conj % 0)) ; Add wall outlet
       (#(conj % (+ 3 (apply max %)))) ; Add device
       sort
       reverse))

(defn differences [joltages]
  (->> (partition 2 1 joltages)
       (map #(apply - %))))

(defn possibilities [m]
  (if (<= m 2) m (+ 7 (* 3 (- m 4)))))

(defn count-perms [joltage-diffs]
  (loop [[current next] (split-with #(not= 3 %) (drop-while #(= 3 %) joltage-diffs))
         perms []]
    (if (empty? current) perms
                         (recur (split-with #(not= 3 %) (drop-while #(= 3 %) next))
                                (conj perms (possibilities (count current)))))))

(comment
  (->> (input "day10.txt")
       (differences)
       (frequencies)
       (#(* (% 3) (% 1))))

  (->> (input "day10.txt")
       differences
       count-perms
       (apply *))

  (possibilities 4)

  (max [1 2 3 4]))
