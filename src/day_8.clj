(ns day-8
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [common :as common]))

(def ^:private width 25)
(def ^:private image-size (* width 6))
(def ^:private layers
  (->> "src/day_8.txt"
       slurp
       (map (comp edn/read-string str))
       (partition image-size)))

(defn part-1 []
  (let [layer-digit-freqs (map frequencies layers)
        least-zeros-layer (apply min-key #(get % 0) layer-digit-freqs)]
    (* (get least-zeros-layer 1)
       (get least-zeros-layer 2))))

(defn first-not-transparent [& pixels]
  (common/find-pred #(not= 2 %) pixels))

(defn part-2 []
  (->> layers
       (apply map first-not-transparent)
       (partition width)
       pprint/pprint))
