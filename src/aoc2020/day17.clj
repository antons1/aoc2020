(ns aoc2020.day17
  (:require [aoc2020.input :as inp]
            [clojure.math.combinatorics :as c]))

(defn coords->keyword [x y z w]
  (keyword (str x "|" y "|" z "|" w)))

(defn keyword->coords [kw]
  (->> (str kw)
       (re-find #"^:(-?\d+)\|(-?\d+)\|(-?\d+)\|(-?\d+)$")
       rest
       (map #(Integer/parseInt %))
       vec))

(defn input [loc]
  (let [input' (inp/read-input loc)]
    {:from  -1
     :to    (inc (count input'))
     :space (->> input'
                 (map-indexed (fn [y line]
                                (->> line (map-indexed #(into {} {(coords->keyword %1 y 0 0) %2})))))
                 flatten
                 (apply merge))}))

(defn get-neighbours
  ([[x y z w] space]
   (get-neighbours x y z w space))
  ([x y z w space]
   (map (fn [coords] ((apply coords->keyword coords) space)) (filter #(not= % [x y z w]) (c/cartesian-product [(- x 1) x (+ x 1)] [(- y 1) y (+ y 1)] [(- z 1) z (+ z 1)] [(- w 1) w (+ w 1)])))))

(defn new-value [kwcoords space value]
  (let [neighbours (get-neighbours (keyword->coords kwcoords) space)
        active-neighbours (count (filter #(= % \#) neighbours))]
    (cond (and (= value \#) (<= 2 active-neighbours 3)) \#
          (and (or (= value \.) (nil? value)) (= 3 active-neighbours)) \#
          :else \.)))

(defn get-coords [from to]
  (let [to' (inc to)]
    (->> (c/cartesian-product (range from to') (range from to') (range from to') (range from to'))
         (map #(apply coords->keyword %)))))

(defn update-map [{:keys [space from to]}]
  {:from (dec from)
   :to (inc to)
   :space (->> (get-coords from to)
               (map (fn [kwcoords] (into {} {kwcoords (new-value kwcoords space (kwcoords space))})))
               (apply merge))})



(comment
  (->> (input "day17.txt")
       update-map
       update-map
       update-map
       update-map
       update-map
       update-map
       :space
       vals
       (filter #(= % \#))
       count)

  (map #(keyword->coords %) (get-coords -1 4))

  (coords->keyword 2 3 4)
  (keyword->coords :-2|3|-4))