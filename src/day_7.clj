(ns day-7
  (:require [clojure.core.async :refer [chan <!! >!! thread]]
            [clojure.math.combinatorics :as combo]
            [common :as common]
            [intcode-computer :as icmp]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_7.txt")))

(defn amplifier [phase-setting]
  (let [{:keys [input-chan output-chan]} (icmp/run {:program     program
                                                    :input-size  2
                                                    :output-size 1})]
    (>!! input-chan phase-setting)
    (fn [input]
      (>!! input-chan input)
      (<!! output-chan))))

(defn execute-feedback [amplifiers-feedback in]
  (loop [[amplifier & rest-amplifiers] amplifiers-feedback
         in in]
    (if-let [out (amplifier in)]
      (recur rest-amplifiers out)
      in)))

(defn amplifiers-feedback [phase-settings]
  (->> phase-settings
       (map amplifier)
       cycle))

(defn highest-signal [phase-settings]
  (->> phase-settings
       combo/permutations
       (map amplifiers-feedback)
       (map #(execute-feedback % 0))
       (apply max)))

(defn part-1 []
  (highest-signal (range 0 5)))

(defn part-2 []
  (highest-signal (range 5 10)))
