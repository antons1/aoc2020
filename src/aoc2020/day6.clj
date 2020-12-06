(ns aoc2020.day6
  (:require [aoc2020.input :as inp]
            [clojure.string :as s]
            [clojure.set :refer [intersection]]))

(defn mapanswers [ansstr]
  (let [persons (s/split-lines ansstr)
        answers (set (s/replace ansstr #"\n" ""))]
    {:persons persons :answers answers}))

(comment
  (->> (inp/read-input "day6.txt" #"\n\n")
       (map mapanswers)
       (map :answers)
       (map count)
       (apply +)) ; Part 1

  (->> (inp/read-input "day6.txt" #"\n\n")
       (map mapanswers)
       (map #(->> % :persons (map set)))
       (map #(apply intersection %))
       (map count)
       (apply +))

  (intersection (set "abc") (set "bcd")))

