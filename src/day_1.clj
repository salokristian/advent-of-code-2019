(ns day-1
  (:require [clojure.edn :as edn]))

(defn get-fuel-mass [mass]
  (- (quot mass 3) 2))

(defn get-fuel-mass-rec [mass]
  (let [fuel-mass (get-fuel-mass mass)]
    (if (< 0 fuel-mass)
      (+ fuel-mass (get-fuel-mass-rec fuel-mass))
      0)))

(defn acc-fuel-mass [get-mass total-mass mass]
  (let [fuel-mass (get-mass mass)]
    (+ fuel-mass total-mass)))

(defn part-1 []
  (->> "day_1.txt"
       (common/parse-file edn/read-string)
       (reduce (partial acc-fuel-mass get-fuel-mass) 0)))

(defn part-2 []
  (->> "day_1.txt"
       (common/parse-file edn/read-string)
       (reduce (partial acc-fuel-mass get-fuel-mass-rec) 0)))
