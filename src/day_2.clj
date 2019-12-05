(ns day-2
  (:require [intcode-computer :as icmp]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_2.txt")))

(defn part-1 []
  (icmp/execute program 0))


(def ^:private inputs
  (for [noun (range 100)
        verb (range 100)]
    [noun verb]))

(defn create-program [[noun verb]]
  (-> program
      (assoc 1 (str noun))
      (assoc 2 (str verb))))

(defn has-output [val program]
  (when (= (first program) val)
    program))

(defn part-2 []
  (->> inputs
       (map create-program)
       (map #(icmp/execute % 0))
       (some (partial has-output "19690720"))))
