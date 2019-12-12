(ns day-12
  (:require [common :as common]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def state
  (->> (common/parse-file #(re-seq #"[-]?\d+" %) "day_12.txt")
       (mapv (comp (partial assoc {:v [0 0 0]} :x)
                   vec
                   (partial map edn/read-string)))))

(defn dv [x0 x1]
  (letfn [(dv-component [p0 p1]
            (cond
              (= p0 p1) 0
              (< p0 p1) 1
              (> p0 p1) -1))]
    (mapv dv-component x0 x1)))

(defn step [state {:keys [v x] :as moon}]
  (let [v' (->> state
                (filter (partial not= moon))
                (map (comp (partial apply dv)
                           (partial vector x)
                           :x))
                (reduce (partial mapv +) v))]
    {:v v'
     :x (mapv + x v')}))

(defn system-energy [state]
  (letfn [(total-energy [moon]
            (* (->> (:x moon) (map math/abs) (reduce +))
               (->> (:v moon) (map math/abs) (reduce +))))]
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
