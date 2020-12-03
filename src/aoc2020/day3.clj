(ns aoc2020.day3
  (:require [aoc2020.input :as inp]))

(defn grid->map [grid]
  {:grid grid :x 0 :y 0 :trees 0})

(defn position [x y {:keys [grid]}]
  (-> grid (nth y) (cycle) (nth x)))

(defn count-trees [inc-x inc-y grid-data]
  (let [max-y (count (:grid grid-data))]
    (loop [{:keys [x y] :as gr} grid-data]
      (let [new-x (+ x inc-x)
            new-y (+ y inc-y)]
        (if (>= new-y max-y)
          gr
          (recur (assoc gr :x new-x
                           :y new-y
                           :trees (+ (:trees gr) (if (= \# (position new-x new-y gr)) 1 0)))))))))

(defn slopes [pairs grid-data]
  (map (fn [[x y]] {:grid-data grid-data :x-inc x :y-inc y}) pairs))

(comment
  (->> (inp/read-input "day3.txt")
      (grid->map)
      (count-trees 3 1)
      (:trees)) ; Part 1

  (->> (inp/read-input "day3.txt")
      (grid->map)
      (slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])
      (map #(count-trees (:x-inc %) (:y-inc %) (:grid-data %)))
      (map #(:trees %))
      (apply *)) ; Part 2

  (nth (cycle "abc") 5)

  (slopes {:a "b" :c "d"} [[4 1] [5 1]]))
