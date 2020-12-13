(ns aoc2020.day13
  (:require [aoc2020.input :as inp]))

;; Part 1
(defn inp->map [[arrival buses]]
  {:arrival   (Integer/parseInt arrival)
   :bus-times (->> (re-seq #"(\d+),?" buses)
                   (map second)
                   (map #(into {} {:interval (Integer/parseInt %)}))
                   vec)})

(defn bus-times->travel [{:keys [arrival] :as travel-data}]
  (update travel-data :bus-times
          (fn [deps]
            (->> deps
                 (map #(assoc % :after (- (:interval %) (mod arrival (:interval %)))))
                 (sort-by :after <)))))

(defn input [loc]
  (->> (inp/read-input loc)
       (inp->map)))

;; Part 2
(defn ->vec [buses]
  (->> buses
       vec
       (map #(try (Integer/parseInt %) (catch Exception _ nil)))
       (map-indexed (fn [idx elem] {:offset idx :time elem}))
       (filter #(some? (:time %)))))
       ;(sort-by :time >)))

(defn inp->map2 [[_ buses]]
  {:timestamp 0
   :bus-intervals (->> (re-seq #"(\d+|x),?" buses)
                       (map second)
                       ->vec)})

(defn run-time [{:keys [bus-intervals]}]
  (loop [inc' 1
         {:keys [offset time] :as current} (first bus-intervals)
         m (mod (* offset -1) time)
         next (rest bus-intervals)
         t 0]
    (cond
      (not= (mod t time) m) (recur inc' current m next (+ t inc'))
      (and (= (mod t time) m) (seq next)) (recur (* inc' time) (first next) (mod (* (:offset (first next)) -1) (:time (first next))) (rest next) t)
      :else t)))

(comment
  (->> (input "day13.txt")
       bus-times->travel
       :bus-times
       first
       (#(* (:interval %) (:after %)))) ;part 1

  (->> (inp/read-input "day13.txt")
       inp->map2
       run-time) ;part 2

  (mod -2 59))
