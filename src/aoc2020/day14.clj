(ns aoc2020.day14
  (:require [aoc2020.input :as inp]
            [clojure.string :as s]))

(defn input' [loc]
  (let [lines (inp/read-input loc)
        mask (->> (first lines) (re-find #"^mask\s=\s(.*)$") second)
        assignments (->> (rest lines) (map #(re-find #"^mem\[(\d+)\]\s=\s(\d+)$" %)) (map (fn [[_ loc val]] [(Integer/parseInt loc) (Integer/parseInt val)])))]
    {:mask mask :assignments assignments :memory (vec (repeat (->> assignments (map first) (apply max)) 0))}))

(defn res->instruction [[_ op loc val]]
  (if (nil? loc) {:op :MASK :loc nil :val val}
                 {:op :MEM :loc (Long/parseLong loc) :val (Long/parseLong val)}))

(defn input [loc]
  (let [instructions (->> (inp/read-input loc)
                          (map #(-> (re-find #"^(mask|mem\[(\d*)\])\s=\s(.*)$" %) res->instruction)))]
    {:mask "" :instructions instructions :memory {}}))

(defn masks [mask]
  (let [xs (->> mask (filter #(= \X %)) count)]
    (loop [xs' (- (Math/pow 2 xs) 1)
           masks []]
      (if (< xs' 0) masks (recur (dec xs') (conj masks (s/replace (format "%36s" (Long/toString xs' 2)) " " "0")))))))

(defn unmask [mask val]
  (let [xs (masks mask)
        val' (s/reverse (s/replace (format "%36s" (Long/toString val 2)) " " "0"))
        parsed-mask
        (->> mask
             vec
             (map-indexed (fn [idx v] (case [v ((vec (s/replace (format "%36s" (Long/toString val 2)) " " "0")) idx)]
                                        [\1 \0] \1
                                        [\0 \1] \0
                                        [\0 \0] \0
                                        [\1 \1] \0
                                        \X)))
             (apply str))]
    (map #(loop [mask' (s/reverse parsed-mask)
                 x (first (reverse %))
                 next (rest (reverse %))]
            (if (nil? (s/index-of mask' "X")) (s/reverse mask')
                                              (recur (s/replace-first mask' #"X" (case [(str (nth val' (s/index-of mask' "X"))) (str x)]
                                                                                   ["1" "0"] "1"
                                                                                   ["0" "1"] "1"
                                                                                   ["0" "0"] "0"
                                                                                   ["1" "1"] "0")) (first next) (rest next)))) xs)))


(defn masked-vals [mask val]
  (->> (unmask mask val)
      (map #(bit-xor val (Long/parseLong % 2)))))


(defn masked-val [mask val]
  (let [ands (-> mask (s/replace #"X" "1") (#(Long/parseLong % 2)))
        ors (-> mask (s/replace #"X" "0") (#(Long/parseLong % 2)))]
    (-> val
        (bit-and ands)
        (bit-or ors))))

(defn initialize [{:keys [mask instructions memory] :as state}]
  (loop [{:keys [op loc val]} (first instructions)
         instructions' (rest instructions)
         memory' memory
         mask' mask]
    (cond
      (= op :MASK) (recur (first instructions') (rest instructions') memory' val)
      (and (= op :MEM) (empty? instructions')) (-> state (assoc :memory (assoc memory' loc (masked-val mask' val))) (assoc :mask mask'))
      :else (recur (first instructions') (rest instructions') (assoc memory' loc (masked-val mask' val)) mask'))))

(defn update-memory [memory idxs val]
  (loop [idx (first idxs)
         next (rest idxs)
         memory' memory]
    (if (empty? next) (assoc memory' idx val)
                      (recur (first next) (rest next) (assoc memory' idx val)))))

(defn initialize' [{:keys [mask instructions memory] :as state}]
  (loop [{:keys [op loc val]} (first instructions)
         instructions' (rest instructions)
         memory' memory
         mask' mask]
    (cond
      (= op :MASK) (recur (first instructions') (rest instructions') memory' val)
      (and (= op :MEM) (empty? instructions')) (-> state (assoc :memory (update-memory memory' (masked-vals mask' loc) val)) (assoc :mask mask'))
      :else (recur (first instructions') (rest instructions') (update-memory memory' (masked-vals mask' loc) val) mask'))))

(comment
  (->> (input "day14.txt")
       initialize
       :memory
       (apply +)) ;part 1

  (->> (input "day14.txt")
       initialize'
       :memory
       vals
       (apply +))

  (s/index-of "00XX00" "X")
  (unmask "000000000000000000000000000000X1001X" 42)
  (masks "000000000000000000000000000000X1001X")
  (->> (masked-vals "000000000000000000000000000000X1001X" 42)
       (map #(Long/toString % 2)))
  (Long/toString 42 2))
