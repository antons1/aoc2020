(ns aoc2020.day18
  (:require [aoc2020.input :as inp]
            [clojure.string :as s]))

(defn ->int|string [val]
  (try (Integer/parseInt (str val))
       (catch Exception _ val)))

(defn input [loc]
  (->> (inp/read-input loc)
      (map #(s/replace % #" " ""))))

(defn walk-line [line]
  (loop [sign (first line)
         signs (rest line)
         result []]
    (cond (or (nil? sign) (= sign \))) [result signs]
          (= sign \() (let [[result' signs'] (walk-line signs)] (recur (first signs') (rest signs') (conj result result')))
          :else (recur (first signs) (rest signs) (conj result sign)))))

(defn give-+-preference [line]
  (if (<= (count line) 3)
    line
    (loop [first' (first line)
           [second' third'] (drop 1 (take 3 line))
           then (drop 3 line)
           result []]
      (cond (and (empty? then) (= second' \+)) (conj result [first' second' third'])
            (empty? then) (conj result first' second' third')
            (= second' \+) (recur [first' second' third'] (take 2 then) (drop 2 then) result)
            :else (recur third' (take 2 then) (drop 2 then) (conj result first' second'))))))

(defn give-+-preference-rec [line]
  (give-+-preference (vec (map #(if (vector? %) (give-+-preference-rec %) %) line))))

(defn op->fn [op]
  (case op
    \+ +
    \* *))

(defn raise-line [line]
  (if (and (vector? line) (= 1 (count line))) (first line) line))

(defn calc-line [line]
  (let [line' (raise-line line)]
    (loop [first' (first line')
           [second' third'] (drop 1 (take 3 line'))
           then (drop 3 line')]
      (prn first' second' third')
      (let [result (case [(vector? first') (vector? third')]
                     [true true] ((op->fn second') (calc-line first') (calc-line third'))
                     [true false] ((op->fn second') (calc-line first') (->int|string third'))
                     [false true] ((op->fn second') (->int|string first') (calc-line third'))
                     [false false] ((op->fn second') (->int|string first') (->int|string third')))]
        (if (empty? then) result
                          (recur result (take 2 then) (drop 2 then)))))))

(comment
  (->> (input "day18.txt")
       (map walk-line)
       (map first)
       (map calc-line)
       (apply +)) ;Part 1

  (->> (input "day18.txt")
       (map walk-line)
       (map first)
       (map give-+-preference-rec)
       (map calc-line)
       (apply +)) ;Part 2
  (raise-line [[:a :b :c]]))

