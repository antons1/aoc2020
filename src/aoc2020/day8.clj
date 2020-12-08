(ns aoc2020.day8
  (:require [aoc2020.input :as inp]))

(defn input->map [inploc]
  (->> (inp/read-input inploc)
       (map #(re-seq #"(^\w{3})\s([+-]?\d*)$" %))
       (map first)
       (map (fn [[_ op arg]] {:op (keyword op) :arg (Integer/parseInt arg) :count 0}))))

(defn next-instruction-index [instruction index instructions]
  (case (:op instruction)
    :jmp (+ index (:arg instruction))
    (+ index 1)))

(defn run-code [instructions]
  (loop [instruction (first instructions)
         index 0
         instructions' instructions
         accumulator 0
         next-index (next-instruction-index instruction index instructions')
         next (nth instructions' next-index)]
    ;(prn "Inst" instruction "acc" accumulator)
    (cond (nil? instruction)         {:reason :done :acc accumulator}
          (= 1 (:count next)) (merge instruction {:reason :panic :acc accumulator :next next})
          :else               (recur
                               next
                               next-index
                               (assoc instructions' index (update instruction :count inc))
                               (if (= :acc (:op instruction)) (+ accumulator (:arg instruction)) accumulator)
                               (next-instruction-index next next-index instructions')
                               (try (nth instructions' (next-instruction-index next next-index instructions')) (catch Exception _ nil))))))

(defn nop-jmp-toggle [{:keys [op] :as instruction}]
  (assoc instruction :op (case op
                           :nop :jmp
                           :jmp :nop
                           op)))


(defn run-code' [instructions]
  (loop [instruction (first instructions)
         index 0
         instructions' instructions
         accumulator 0
         next-index (next-instruction-index instruction index instructions')
         next (nth instructions' next-index)]
    ;(prn "Inst" instruction "acc" accumulator)
    (cond (nil? instruction)         {:reason :done :acc accumulator}
          (= 1 (:count next)) instructions'
          :else               (recur
                                next
                                next-index
                                (assoc instructions' index (update instruction :count inc))
                                (if (= :acc (:op instruction)) (+ accumulator (:arg instruction)) accumulator)
                                (next-instruction-index next next-index instructions')
                                (try (nth instructions' (next-instruction-index next next-index instructions')) (catch Exception _ nil))))))

(defn run-debug' [terminated instructions]
  (->> terminated
       (map (fn [t-instruction] (run-code (assoc instructions (:index t-instruction) (nop-jmp-toggle (assoc t-instruction :count 0))))))
       (filter #(= (:reason %) :done))))


(comment
  (->> (input->map "test/day8.txt")
       vec
       (run-code))

  (let [instructions (->> (input->map "day8.txt") vec)
        terminated (->> instructions
                        run-code'
                        (map-indexed #(assoc %2 :index %1))
                        reverse
                        vec)
        fix (run-debug' terminated instructions)]
        ;fix' (nop-jmp-toggle (assoc fix :count 0))]
    fix)


  (count [1 2 3 4])
  (nth [1 2 3 4] (- (count [1 2 3 4]) 1)))

       ;run-code))

