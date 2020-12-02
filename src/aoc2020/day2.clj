(ns aoc2020.day2
  (:require [aoc2020.input :as inp]))

(defn ->map [[_ min max letter pwd]]
  {:min (read-string min) :max (read-string max) :letter letter :password pwd})

(defn input->map [input]
  (->> input
    (map #(re-find #"(\d{1,3})-(\d{1,3})\s(.):\s(.*)" %))
    (map #(->map %))))

(defn valid-pwd-sled? [{:keys [min max letter password]}]
  (let [matches (re-seq (re-pattern letter) password)
        occurences (count matches)]
    (<= min occurences max)))

(defn nth-letter [haystack n]
  (str (nth haystack (- n 1))))

(defn valid-pwd-toboggan? [{:keys [min max letter password]}]
  (let [first-pos (= letter (nth-letter password min))
        second-pos (= letter (nth-letter password max))]
    (and
      (or first-pos second-pos)
      (not (and first-pos second-pos)))))

(defn valid-pwd-count [input predicate]
  (->> input
     (input->map)
     (filter #(predicate %))
     (count)))

(comment
  (valid-pwd-count (inp/read-input "day2.txt") valid-pwd-sled?) ;Part 1
  (valid-pwd-count (inp/read-input "day2.txt") valid-pwd-toboggan?) ;Part 2
  (valid-pwd-sled? {:min 5, :max 6, :letter "d", :password "jdddqqtdd"})
  (nth-letter "abcdefg" 1)
  (valid-pwd-toboggan? {:min 5 :max 6 :letter "d" :password "aaaaaakk"}))

