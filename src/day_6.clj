(ns day-6
  (:require [clojure.string :as str]
            [common :as common]))

(def ^:private orbit-map
  (->> "day_6.txt"
       (common/parse-file #(str/split % (re-pattern "\\)")))
       (reduce (fn [map [center orbiter]]
                 (update map center #(if (nil? %)
                                       (vector orbiter)
                                       (conj % orbiter))))
               {})))

(defn stack-entries [orbiters route-to]
  (mapv (fn [planet] {:planet   planet
                      :route-to route-to})
        orbiters))

(defn routes
  [orbit-tree start]
  (loop [stack (stack-entries (get orbit-tree start) [start])
         all-routes []]
    (if (empty? stack)
      all-routes
      (let [{:keys [route-to planet] :as popped} (peek stack)
            orbiters (stack-entries (get orbit-tree planet) (conj route-to planet))]
        (recur (into (pop stack) orbiters)
               (conj all-routes popped))))))

(defn find-route [routes planet]
  (->> routes
       (common/find-pred #(= planet (:planet %)))
       :route-to))

(defn part-1 []
  (let [direct-orbits (->> orbit-map vals flatten count)
        routes (routes orbit-map "COM")
        indirect-orbits (map (comp dec count :route-to) routes)]
    (+ direct-orbits
       (reduce + indirect-orbits))))

(defn part-2 []
  (let [routes (routes orbit-map "COM")
        my-route (find-route routes "YOU")
        santas-route (find-route routes "SAN")
        closest-common-orbit (common/find-pred (partial (complement contains?)
                                                        (set santas-route))
                                               my-route)
        closest-common-orbit-route (find-route routes closest-common-orbit)]
    (+ (count my-route)
       (count santas-route)
       (* -2 (count closest-common-orbit-route)))))
