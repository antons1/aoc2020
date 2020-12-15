(ns aoc2020.day15)

(defn num-or-0 [numbers turn current]
  (try (- turn (numbers current))
       (catch NullPointerException _ 0)))

(defn run-until [init stop]
  (loop [turn 1
         numbers {}
         init' init]
    ;(prn turn numbers init')
    (cond (seq (rest init')) (recur (inc turn) (assoc numbers (first init') turn) (rest init'))
          (= turn stop) (first init')
          :else (recur (inc turn) (assoc numbers (first init') turn) [(num-or-0 numbers turn (first init'))]))))

(comment
  (run-until [1 0 16 5 17 4] 2020) ;Part 1
  (run-until [1 0 16 5 17 4] 30000000)) ;Part 2
