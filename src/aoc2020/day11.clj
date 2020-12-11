(ns aoc2020.day11
  (:require [aoc2020.input :as inp]))

(defn input [location]
  (->> (inp/read-input location)
       (map vec)
       vec))

(defn get-seat [x y seats]
  (try (-> seats (nth y) (nth x))
       (catch IndexOutOfBoundsException _ nil)))

(defn first-seat-in-direction [x y xchange ychange seats]
  (loop [x' (+ x xchange)
         y' (+ y ychange)]
    (if (not= (get-seat x' y' seats) \.) (get-seat x' y' seats)
                               (recur (+ x' xchange) (+ y' ychange)))))

(defn seats-around [x y seats]
  (loop [x' (- x 1)
         y' (- y 1)
         around []]
    (cond (= y' (+ y 2)) around
          (and (= x' x) (= y' y)) (recur (inc x') y around)
          :else (recur (if (= x' (+ x 1)) (- x 1) (inc x'))
                       (if (= x' (+ x 1)) (inc y') y')
                       (conj around (get-seat x' y' seats))))))

(defn visible-seats-around [x y seats]
  (->> [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]]
       (map (fn [[xchange ychange]] (first-seat-in-direction x y xchange ychange seats)))))

(defn should-fill? [seats-around seat]
  (and (= seat \L)
       (->> seats-around
            (filter #(= \# %))
            empty?)))

(defn should-clear? [seat-tolerance seats-around seat]
  (and (= seat \#)
       (->> seats-around
            (filter #(= \# %))
            count
            (<= seat-tolerance))))

(defn run-seats [around-fn seat-tolerance seats]
  (let [xlen (count (first seats))
        ylen (count seats)]
    (loop [x 0
           y 0
           seats' seats]
      (if (= y ylen)
        seats'
        (let [around (around-fn x y seats) ;Rules are applied simultaneously to all seats
              seat (get-seat x y seats)] ; e.g. use input state when deciding what to change
          (recur (if (= x (- xlen 1)) 0 (inc x))
                 (if (= x (- xlen 1)) (inc y) y)
                 (cond (should-fill? around seat) (assoc-in seats' [y x] \#)
                       (should-clear? seat-tolerance around seat) (assoc-in seats' [y x] \L)
                       :else seats')))))))

(defn run-until-stable [around-fn seat-tolerance seats]
  (loop [prev-state seats
         seats' (run-seats around-fn seat-tolerance seats)]
    (if (= seats' prev-state) seats'
                              (recur seats' (run-seats around-fn seat-tolerance seats')))))


(comment
  (->> (input "day11.txt")
       (run-until-stable seats-around 4)
       flatten
       (filter #(= \# %))
       count) ;Part 1

  (->> (input "day11.txt")
       (run-until-stable visible-seats-around 5)
       flatten
       (filter #(= \# %))
       count) ;Part 2

  (should-clear? [\# \# \# \L \. \. \L \L]))
