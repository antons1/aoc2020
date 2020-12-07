(ns aoc2020.day7
  (:require [aoc2020.input :as inp]
            [clojure.string :as s]))

(defn color->keyword [color]
  (-> color (s/replace #" " "") keyword))

(defn contains->map [[_ amount color]]
  {:amount (Integer/parseInt amount) :color (color->keyword color)})

(defn rule->map [rule]
  (let [[_ color] (re-find #"^(\w+ \w+)" rule)
        contains (re-seq #"\s(\d)\s(\w+\s\w+)\sbags?" rule)]
    {:color (color->keyword color) :contains (->> contains
                                                  (map contains->map)
                                                  vec)}))

(defn can-contain-color? [color rule]
  (->> (:contains rule)
       (filter #(= (:color %) color))
       seq))

(defn can-contain-color-nested? [color rule rules]
  (cond (empty? (:contains rule)) false
        (can-contain-color? color rule) true
        :else (->> (:contains rule)
                   (map #(can-contain-color-nested? color ((:color %) rules) rules))
                   (reduce #(or %1 %2)))))

(defn possible-outer-bags [color rules]
  (filter #(can-contain-color-nested? color (last %) rules) rules))

(defn needs-amount-bags
  ([color rules]
   (needs-amount-bags color (color rules) rules))
  ([color rule rules]
   (if (empty? (:contains rule))
     0
     (->> (:contains rule)
          (map #(+ (:amount %)
                   (* (:amount %) (needs-amount-bags color ((:color %) rules) rules))))
          (apply +)))))


(comment
  (->> (inp/read-input "day7.txt")
       (map rule->map)
       (reduce #(assoc %1 (:color %2) %2) {})
       (possible-outer-bags :shinygold)
       count) ; Part 1

  (->> (inp/read-input "day7.txt")
       (map rule->map)
       (reduce #(assoc %1 (:color %2) %2) {})
       (needs-amount-bags :shinygold)) ; Part 2

  (can-contain-color-nested? :lightred {:color :shinygold, :contains [{:amount 1, :color :darkolive} {:amount 2, :color :vibrantplum}]} rules)
  (filter prn {:a "b" :c "d"})
  (needs-amount-bags :shinygold rules))

