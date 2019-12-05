(ns day-5
  (:require [intcode-computer :as icmp]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_5.txt")))

(defn part-1 []
  (icmp/execute program 0)
  "DONE")
