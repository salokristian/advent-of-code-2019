(ns day-3
  (:require [clojure.edn :as edn]
            [clojure.set :as clj-set]
            [clojure.string :as str]))

(defn parse-wire [s]
  (->> (str/split s #",")
       (map (fn [s]
              {:dir   (subs s 0 1)
               :steps (edn/read-string (subs s 1))}))))

(defn line-points [start-point line]
  (let [step-line (case (:dir line)
                    "U" #(update start-point :y + %)
                    "D" #(update start-point :y - %)
                    "R" #(update start-point :x + %)
                    "L" #(update start-point :x - %))]
    (->> (:steps line)
         inc
         (range 1)
         (map step-line))))

(defn create-grid [wire]
  (reduce (fn [grid line]
            (into grid (line-points (last grid) line)))
          [{:x 0 :y 0}]
          wire))

(def wires (common/parse-file parse-wire "day_3.txt"))
(def grid-1 (create-grid (first wires)))
(def grid-2 (create-grid (second wires)))
(def intersect-points (clj-set/intersection (set grid-1) (set grid-2)))

(defn part-1 []
  (->> intersect-points
       (map #(+ (Math/abs (:x %)) (Math/abs (:y %))))
       (filter #(not (zero? %)))
       (apply min)))

(defn part-2 []
  (->> intersect-points
       (map #(+ (.indexOf grid-1 %) (.indexOf grid-2 %)))
       (filter #(not (zero? %)))
       (apply min)))
