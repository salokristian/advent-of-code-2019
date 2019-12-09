(ns day-9
  (:require [clojure.core.async :as async :refer [<!! >!! thread chan]]
            [intcode-computer :as icmp]
            [common :as common]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_9.txt")))

(defn run [input]
  (let [{:keys [input-chan output-chan]} (icmp/run {:program     program
                                                    :input-size  1
                                                    :output-size 100})]
    (>!! input-chan input)
    (<!! (async/into [] output-chan))))

(defn part-1 [] (run 1))
(defn part-2 [] (run 2))
