(ns day-15
  (:require [clojure.core.async :refer [<!! >!!]]
            [intcode-computer :as icmp]
            [common :as common])
  (:import clojure.lang.PersistentQueue))

(def program-code (first (common/parse-file icmp/parse "day_15.txt")))
(def program (icmp/run {:program     program-code
                        :input-size  1
                        :output-size 1}))

(defn move-droid! [dir]
  (>!! (:input-chan program) dir)
  (<!! (:output-chan program)))

(defn move [{x1 :x y1 :y} {x2 :x y2 :y}]
  (cond
    (< x1 x2) 4
    (> x1 x2) 3
    (< y1 y2) 2
    (> y1 y2) 1
    :else (throw (Exception. "Invalid move"))))

(def possible-moves
  [{:x 1 :y 0}
   {:x -1 :y 0}
   {:x 0 :y 1}
   {:x 0 :y -1}])

(defn move-back-forth! [from to]
  (let [status (move-droid! (move from to))]
    (when-not (zero? status)
      (move-droid! (move to from)))
    status))

(defn move-droid-to! [path {:keys [prev] :as cur}]
  (let [[path' common-ancestor-path] (split-with (partial not= prev) path)
        move-path (-> common-ancestor-path reverse vec (conj cur))
        moves (->> move-path rest (map move move-path))]
    (run! move-droid! moves)
    (into (vec path') (if prev
                        [prev (select-keys cur [:x :y])]
                        [(select-keys cur [:x :y])]))))

(defn visit-adjacent! [cur visited]
  (let [adjacent (->> possible-moves
                      (map (partial merge-with +) (repeat cur))
                      (map #(assoc % :prev (select-keys cur [:x :y])))
                      (filter (complement #(get-in visited [(:y %) (:x %)]))))
        cells (->> adjacent
                   (map (partial move-back-forth! cur))
                   (map #(assoc %1 :type %2) adjacent))]
    {:wall      (filter #(= (% :type) 0) cells)
     :visitable (filter (complement #(= (% :type) 0)) cells)}))

(defn assoc-visited [visited cells]
  (reduce (fn [visited {:keys [x y type]}]
            (assoc-in visited [y x] type))
          visited
          cells))

(defn dfs [start]
  (loop [stack [start]
         path []
         visited {}]
    (if (empty? stack)
      visited
      (let [cur (peek stack)
            path' (move-droid-to! path cur)
            {:keys [wall visitable]} (visit-adjacent! cur visited)]
        (recur (into (pop stack) visitable)
               path'
               (assoc-visited visited (conj wall cur)))))))

(def system-map (dfs {:x 0 :y 0 :type 1}))

(defn neighbors [system-map cell]
  (let [cell (select-keys cell [:x :y])]
    (->> possible-moves
         (map (partial merge-with + cell))
         (map #(assoc % :type (get-in system-map [(:y %) (:x %)])))
         (filter #(pos? (:type %))))))

(defn graph-bfs
  [graph v]
  (loop [queue (conj PersistentQueue/EMPTY v)
         visited #{}]
    (let [{:keys [dist] :as v} (peek queue)
          neighbors (neighbors graph v)
          not-visited (->> neighbors
                           (filter (complement visited))
                           (map #(assoc % :dist (inc dist))))
          new-queue (apply conj (pop queue) not-visited)]
      (if (= 2 (:type v))
        (:dist v)
        (recur new-queue (conj visited (select-keys v [:x :y :type])))))))
