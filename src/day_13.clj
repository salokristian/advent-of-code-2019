(ns day-13
  (:require [clojure.core.async :as async :refer [<!! >!!]]
            [intcode-computer :as icmp]
            [common :as common]))

(def program
  (let [program (first (common/parse-file icmp/parse "day_13.txt"))]
    ;(assoc program 0 "2")
    program))

(def tile-instructions
  (let [{:keys [output-chan]} (icmp/run {:program     program
                                         :input-size  nil
                                         :output-size nil})]
    (<!! (async/into [] output-chan))))

(def tiles
  (->> tile-instructions
       (partition 3)
       (map (fn [[x y id]] {y {x id}}))
       (apply merge-with merge)))

(defn part-1 []
  (->> (vals tiles)
       (map vals)
       flatten
       frequencies))
