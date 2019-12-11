(ns day-11
  (:require [clojure.core.async :refer [<!! >!!]]
            [intcode-computer :as icmp]
            [common :as common]))

(def ^:private program
  (first (common/parse-file icmp/parse "day_11.txt")))

(defn next-dir [dir turn]
  (let [dirs [:up :right :down :left]
        dir-order (case turn
                    0 reverse
                    1 identity)]
    (->> dirs dir-order cycle (drop-while #(not= dir %)) second)))

(defn next-pos [pos dir]
  (case dir
    :up (update pos 0 dec)
    :right (update pos 1 inc)
    :down (update pos 0 inc)
    :left (update pos 1 dec)))

(defn update-state [{:keys [pos dir robot-map]} {:keys [turn color]}]
  (let [dir' (next-dir dir turn)
        pos' (next-pos pos dir')]
    {:pos       pos'
     :dir       dir'
     :color     color
     :robot-map (update-in robot-map pos (partial cons color))}))

(defn robot [{:keys [input-chan output-chan robot-map pos] :as params}]
  (let [color (-> robot-map (get-in pos) first (or 0))]
    (>!! input-chan color))
  (let [[color turn] [(<!! output-chan) (<!! output-chan)]]
    (if (some? turn)
      (recur (merge params
                    (update-state params {:turn turn :color color})))
      params)))

(defn run [initial-color]
  (let [{:keys [input-chan output-chan]} (icmp/run {:program     program
                                                    :input-size  1
                                                    :output-size 100})
        params {:input-chan  input-chan
                :output-chan output-chan
                :pos         [0 0]
                :dir         :up
                :robot-map   {0 {0 (list initial-color)}}}]
    (:robot-map (robot params))))

(defn print-robot-map [robot-map]
  (let [xs (->> robot-map vals (mapcat keys) distinct)
        ys (keys robot-map)
        [x-min x-max] [(apply min xs) (apply max xs)]
        [y-min y-max] [(apply min ys) (apply max ys)]]
    (doseq [y (range y-min (inc y-max))
            x (range x-min (inc x-max))
            :let [color (first (get-in robot-map [y x]))
                  letter (if (= color 1) "#" ".")
                  line-end? (= x x-max)]]
      (if line-end?
        (println letter)
        (print letter " ")))))

(defn part-1 []
  (->> (run 0)
       vals
       (map count)
       (reduce +)))

(defn part-2 []
  (let [robot-map (run 1)]
    (print-robot-map robot-map)))

