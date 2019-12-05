(ns day-5
  (:require [intcode-computer :as icmp]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_5.txt")))

(defn run []
  (icmp/execute {:program program :counter 0})
  nil)
