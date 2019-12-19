(ns day-19
  (:require [clojure.core.async :refer [<!! >!!]]
            [intcode-computer :as icmp]
            [common :as common]))


(def program-code (first (common/parse-file icmp/parse "day_19.txt")))
(defn program [] (icmp/run {:program     program-code
                            :input-size  2
                            :output-size 1}))

(def beam-coords
  (for [y (range 70)
        x (range 50)
        :let [program (program)]
        :when (do (>!! (:input-chan program) x)
                  (>!! (:input-chan program) y)
                  (= 1 (<!! (:output-chan program))))]
    {:x x
     :y y}))

(defn part-1 []
  (count beam-coords))

(defn plot-beam []
  (doseq [y (range 70)
          x (range 50)
          :let [status (if (some (partial = {:x x :y y}) beam-coords) "#" ".")
                line-end? (= x 49)]]
    (if line-end?
      (println status)
      (print status " "))))
