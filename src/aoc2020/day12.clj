(ns aoc2020.day12
  (:require [aoc2020.input :as inp]))

(defn input [loc]
  (->> (inp/read-input loc)
       (map #(re-find #"^(\w)(\d*)$" %))
       (map #(into {} {:dir (keyword (% 1)) :len (Integer/parseInt (% 2))}))
       ((fn [inst] {:direction :E :E 10 :N 1 :ship {:E 0 :N 0} :instructions inst}))))

(defn turn-direction [state-dir turn-len turn-dir]
  (let [directions [:N :E :S :W]
        directions' (cycle (if (= turn-dir :L) (reverse directions) directions))]
    (nth directions' (+ (/ turn-len 90) (.indexOf directions' state-dir)))))

(defn state-direction [direction]
  (cond (= direction :W) :E
        (= direction :S) :N
        :else direction))

(defn state-mod-fn [direction]
  (if (#{:W :S} direction) - +))

(defn move-ship [ship we wn len]
  (-> ship
      (update :E #(+ % (* we len)))
      (update :N #(+ % (* wn len)))))

(defn turn-waypoint [we wn turn-dir turn-len]
  (let [directions [{:E wn :N (* we -1)} {:E (* we -1) :N (* wn -1)} {:E (* wn -1) :N we}]
        directions' (cycle (if (= turn-dir :L) (reverse directions) directions))]
    (nth directions' (- (/ turn-len 90) 1))))

(defn travel [{:keys [direction E N instructions] :as state}]
  (let [{:keys [dir len]} (first instructions)
        state' (cond (or (= dir :L) (= dir :R)) (merge state (turn-waypoint E N dir len))
                     (= dir :F) (update state :ship #(move-ship % E N len))
                     :else (update state (state-direction dir) #((state-mod-fn dir) % len)))]
    (update state' :instructions rest)))

(defn travel-all [state]
  (loop [{:keys [instructions] :as state'} state]
    ;(prn state')
    (if (empty? instructions) state'
                              (recur (travel state')))))

(comment
  (-> (input "day12.txt")
      travel-all
      (#(+ (Math/abs (:E %)) (Math/abs (:N %))))) ;Part 1 (does not work anymore)

  (-> (input "day12.txt")
      travel-all
      (#(+ (Math/abs (-> % :ship :E)) (Math/abs (-> % :ship :N))))) ;Part 2

  (#{:E :W} :N)
  (turn-waypoint 10 4 :R 90))