(ns day-12
  (:require [common :as common]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def state
  (->> (common/parse-file #(re-seq #"[-]?\d+" %) "day_12.txt")
       (mapv (comp #(mapv vector % [0 0 0])
                   (partial map edn/read-string)))))

(defn dv [x0 x1]
  (letfn [(dv-component [p0 p1]
            (cond
              (= p0 p1) 0
              (< p0 p1) 1
              (> p0 p1) -1))]
    (mapv dv-component x0 x1)))

(defn step [state moon]
  (let [x (mapv first moon)
        v (mapv second moon)
        v' (->> state
                (filter (partial not= moon))
                (map (comp (partial dv x)
                           (partial mapv first)))
                (reduce (partial mapv +) v))
        x' (mapv + x v')]
    (mapv vector x' v')))

(defn system-energy [state]
  (letfn [(total-energy [moon]
            (* (->> moon (map first) (map math/abs) (reduce +))
               (->> moon (map second) (map math/abs) (reduce +))))]
    (->> state
         (map total-energy)
         (reduce +))))

(defn part-1 [steps]
  (loop [state state
         steps steps]
    (if (zero? steps)
      (system-energy state)
      (recur (mapv (partial step state) state)
             (dec steps)))))

(defn to-axis [state]
  (apply map vector state))

(defn part-2 []
  (let [axis-state (to-axis state)]
    (loop [steps 0
           state state
           periods (take (count axis-state) (repeat nil))]
      (let [steps' (inc steps)
            state' (mapv (partial step state) state)
            axis-full-period? (map = axis-state (to-axis state'))
            periods' (mapv #(if %2 (or %1 steps') %1) periods axis-full-period?)]
        (if (every? some? periods')
          (reduce math/lcm periods')
          (recur steps' state' periods'))))))
