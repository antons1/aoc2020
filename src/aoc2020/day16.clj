(ns aoc2020.day16
  (:require [aoc2020.input :as inp]))

(defn ticket->list [ticket]
  (->> ticket (re-seq #"(\d+),?") (map #(-> % second (Integer/parseInt)))))

(defn tickets->lists [tickets]
  (->> tickets rest (map ticket->list)))

(defn rule->map [[name from1 to1 from2 to2]]
  {:name name
   :ranges [[(Integer/parseInt from1) (Integer/parseInt to1)]
            [(Integer/parseInt from2) (Integer/parseInt to2)]]})


(defn rules->map [rules]
  (->> rules (map #(re-find #"^(.+):\s(\d+)-(\d+)\sor\s(\d+)-(\d+)$" %))
       (map #(rule->map (rest %)))))


(defn input->map [[rules your-ticket nearby-tickets]]
  {:rules          (rules->map rules)
   :your-ticket    (ticket->list (second your-ticket))
   :nearby-tickets (tickets->lists nearby-tickets)})

(defn input [loc]
  (->> (inp/read-input loc)
       (partition-by #(= "" %))
       (filter #(not= "" (first %)))
       input->map))

(defn valid-for-rule? [[min max] value]
  (<= min value max))

(defn valid-for-any-rule? [rules value]
  (reduce #(or %1 (valid-for-rule? %2 value)) false rules))

(defn invalid-rules [{:keys [rules nearby-tickets] :as state}]
  (let [rules' (->> rules (mapcat :ranges))]
    (assoc state :invalid (->> nearby-tickets
                               (map (fn [values] (->> values (map #(if (valid-for-any-rule? rules' %) nil %)))))
                               flatten
                               (filter some?)))))

(defn valid-rules [{:keys [rules nearby-tickets] :as state}]
  (let [rules' (->> rules (mapcat :ranges))]
    (assoc state :valid-tickets (->> nearby-tickets
                                     (filter #(true? (reduce (fn [p v] (and p (valid-for-any-rule? rules' v))) true %)))
                                     (apply mapv vector)))))

(defn valid-for-rules [rules values]
  (filter #(->> (map (fn [value] (valid-for-any-rule? (:ranges %) value)) values)
                (reduce (fn [a b] (and a b))))
          rules))

(defn field-names' [{:keys [rules valid-tickets] :as state}]
  (->> (loop [rule-matches (->> (map-indexed #(into {} {%1 (valid-for-rules rules %2)}) valid-tickets) (apply merge))
              removeable (->> rule-matches vals (filter #(= (count %) 1)) first)
              filterable (filter #(> (count (second %)) 1) rule-matches)]
         (if (= 1 (->> rule-matches vals (map count) (apply max)))
           rule-matches
           (let [rule-matches' (merge rule-matches
                                      (apply merge (map (fn [[idx rules]] (into {} {idx (filter (fn [rule] (not (some #(= rule %) removeable))) rules)})) filterable)))]
             (recur rule-matches'
                    (->> rule-matches' vals (filter #(= (count %) 1)) (map first))
                    (filter #(> (count (second %)) 1) rule-matches')))))
       (sort-by first)
       (map #(-> % second first :name))
       (assoc state :fields)))

(defn name-ticket [{:keys [your-ticket fields] :as state}]
  (assoc state :your-ticket (->> (map (fn [a b] {b a}) your-ticket fields) (apply merge))))

(comment
  (->> (input "day16.txt")
       invalid-rules
       :invalid
       (apply +))

  (->> (input "day16.txt")
       valid-rules
       field-names'
       name-ticket
       :your-ticket
       (filter #(re-find #"^departure" (first %)))
       (map second)
       (apply *))

  (valid-for-any-rule? [[3 6] [9 10]] 2))
