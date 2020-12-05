(ns aoc2020.day5
  (:require [aoc2020.input :as inp]))

(defn l->kw [letters]
  (map #(cond (or (= % \B) (= % \R)) :upper
              (or (= % \F) (= % \L)) :lower) letters))

(defn find-place [splits min max]
  (loop [positions (range min max)
         split (first splits)
         next (rest splits)]
    (cond (empty? next) (if (= split :lower) (first positions) (last positions))
          (= split :lower) (recur (first (partition (/ (count positions) 2) positions)) (first next) (rest next))
          (= split :upper) (recur (last (partition (/ (count positions) 2) positions)) (first next) (rest next)))))

(defn letters->position [letters min max]
  (-> letters
      l->kw
      (find-place min max)))

(defn seat->map [boarding-str]
  (let [row  (letters->position (take 7 boarding-str) 0 128)
        seat (letters->position (take-last 3 boarding-str) 0 8)]
    {:row row :seat seat :id (+ (* row 8) seat)}))

(defn missing-seat [seats]
  (loop [seat (first seats)
         next (rest seats)]
    (if (= (- (first next) seat) 2) [seat (first next)]
                                    (recur (first next) (rest next)))))

(comment
  (->> (inp/read-input "day5.txt")
       (map seat->map)
       (map :id)
       (apply max)) ; Part 1

  (->> (inp/read-input "day5.txt")
       (map seat->map)
       (map :id)
       sort
       missing-seat) ; Part 2

  (l->kw '(\F \B \F))

  (find-place (l->kw '(\F \B \F \B \B \F \F)) 0 128)
  (find-place (l->kw '(\B \B \B \B \B \B \B)) 0 128)
  (partition (/ (count (range 0 128)) 2) (range 0 128)))
