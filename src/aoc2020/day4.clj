(ns aoc2020.day4
  (:require [aoc2020.input :as inp]))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.
;cid (Country ID) - ignored, missing or not

(defn inp->map [input]
  (->> input
       (map #(re-seq #"(\w*):([\w#]*)[\n\s]?" %))
       (map #(->> %
                  (map (fn [[_ k v]] {(keyword k) v}))
                  (apply merge)))))

(defn parse-or-nil [string]
  (try (Integer/parseInt string)
       (catch Exception _ nil)))

(defn contains-passport-keys? [passport]
  (every? (fn [k] (contains? passport k)) [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(defn valid-number? [numstring min max]
  "Check that string can be parsed, and it is between min and max (inclusive)"
  (if-let [num (parse-or-nil numstring)]
    (<= min num max)
    false))

(defn valid-height? [hgt]
  (let [[_ height unit] (first (re-seq #"^(\d*)(cm|in)$" hgt))]
    (cond (= unit "cm") (valid-number? height 150 193)
          (= unit "in") (valid-number? height 59 76)
          :default false)))

(defn valid-passport? [{:keys [byr iyr eyr hgt hcl ecl pid] :as passport}]
  (and (contains-passport-keys? passport)
       (valid-number? byr 1920 2002)
       (valid-number? iyr 2010 2020)
       (valid-number? eyr 2020 2030)
       (valid-height? hgt)
       (re-find #"^#[0-9a-f]{6}$" hcl)
       (re-find #"^(amb|blu|brn|gry|grn|hzl|oth)$" ecl)
       (re-find #"^\d{9}$" pid)))

(comment
  (->> (inp->map (inp/read-input "day4.txt" #"\n\n"))
       (filter #(contains-passport-keys? %))
       (count)) ;Part 1

  (->> (inp->map (inp/read-input "day4.txt" #"\n\n"))
       (filter #(valid-passport? %))
       (count)) ;Part 2

  (valid-passport? {:pid "087499704" :hgt "74in" :ecl "grn" :iyr "2012" :eyr "2030" :byr "1980" :hcl "#623a2f"})
  (valid-height? "150cm")
  (re-find #"#[1-9a-f]{6}$" "#fffff")
  (re-find #"^(amb|blu|brn|gry|grn|hzl|oth)$" "brg")
  (re-find #"^\d{9}$" "999999999"))
