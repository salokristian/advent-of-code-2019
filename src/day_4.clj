(ns day-4)

(defn matching-digits? [count-comp digits]
  (->> digits
       (partition-by identity)
       (some #(count-comp 2 (count %)))))

(defn valid-password? [matching-count-comp password]
  (let [pw-digits (common/digits password)]
    (and (apply <= pw-digits)
         (matching-digits? matching-count-comp pw-digits))))

(def passwords (range 147981 691424))

(defn valid-pw-count [matching-count-comp]
  (->> passwords
       (map (partial valid-password? matching-count-comp))
       (filter true?)
       count))

(defn part-1 []
  (valid-pw-count <=))

(defn part-2 []
  (valid-pw-count =))
