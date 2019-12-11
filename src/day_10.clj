(ns day-10
  (:require [clojure.string :as str]
            [common :as common]))


(def asteroids (->> (slurp "src/day_10.txt")
                    str/split-lines
                    (map-indexed (fn [y row]
                                   (map-indexed (fn [x val]
                                                  {:x x, :y y, :empty? (= val \.)})
                                                row)))
                    flatten
                    (filter (complement :empty?))
                    (map #(select-keys % [:x :y]))))

(defn vec-length [[x y]]
  (Math/sqrt (+ (Math/pow x 2)
                (Math/pow y 2))))

(defn angle [p0 p1]
  (let [[x y :as v] [(- (:x p1) (:x p0)) (- (:y p1) (:y p0))]
        len (vec-length v)
        ; Rotate the axis to get the clockwise angle from negative (upwards) y-axis
        x-unit (/ y len)
        y-unit (* -1 (/ x len))]
    (common/round-places (Math/atan2 y-unit x-unit) 8)))

(defn dist [p0 p1]
  (let [v [(- (:x p1) (:x p0)) (- (:y p1) (:y p0))]]
    (vec-length v)))

(defn blocked-asteroids [asteroids point]
  (->> asteroids
       (filter (partial not= point))
       (map (partial angle point))
       frequencies
       vals
       (map dec)
       (reduce +)))

(defn part-1 []
  (let [max-visible-asteroids (-> asteroids count dec)
        blocked-asteroids (map (partial blocked-asteroids asteroids) asteroids)]
    (->> (map #(assoc %1 :visible (- max-visible-asteroids %2)) asteroids blocked-asteroids)
         (apply max-key :visible))))

(defn part-2 [n]
  (let [laser-point (select-keys (part-1) [:x :y])]
    (->> asteroids
         (filter (partial not= laser-point))
         (group-by (partial angle laser-point))
         (map (fn [[k v]] [k (sort-by (partial dist laser-point) v)]))
         (into (sorted-map))
         vals
         (apply interleave)
         (drop (dec n))
         first)))
