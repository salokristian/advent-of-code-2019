(ns day-14
  (:require [clojure.string :as str]
            [common :as common]
            [clojure.edn :as edn]))

(defn parse-reaction [s]
  (->> (str/split s #"=>")
       (map (comp (partial into {})
                  (partial map (fn [[_ amount chemical]] [chemical (edn/read-string amount)]))
                  (partial re-seq #"(\d+) ([A-Z]+)")))
       (zipmap [:in :out])))

(def reactions (common/parse-file parse-reaction "day_14.txt"))
(def ore-supply 1000000000000)

(defn find-reaction [reactions chemical]
  (letfn [(reactant [reaction]
            (common/find-pred (fn [[chem]] (= chem chemical)) (:out reaction)))]
    (let [reaction (common/find-pred reactant reactions)]
      {:reaction reaction
       :produces (second (reactant reaction))})))

(defn multiply [reactants x]
  (->> reactants (map (fn [[k v]] [k (* x v)])) (into {})))

(defn get-requirements [reactions requirements]
  (loop [requirements requirements]
    (if-let [[chemical amount] (common/find-pred (fn [[chemical amount]]
                                                   (and (pos-int? amount)
                                                        (not= "ORE" chemical))) requirements)]
      (let [{:keys [reaction produces]} (find-reaction reactions chemical)
            multiple (+ (quot amount produces) (if-not (zero? (mod amount produces)) 1 0))
            requirements' (merge-with + requirements
                                      (multiply (:out reaction) (* -1 multiple))
                                      (multiply (:in reaction) multiple))]
        (recur requirements'))
      requirements)))

(defn get-ore [fuel]
  (get (get-requirements reactions
                         {"FUEL" fuel})
       "ORE"))

(defn binary-search [f goal low high]
  (loop [low low
         high high
         probes '()]
    (if (< high low)
      {:res    nil
       :probes probes}
      (let [mid (quot (+ low high) 2)
            res (f mid)
            latest-res' (conj probes {:probe mid :res res})]
        (cond
          (< res goal) (recur (inc mid) high latest-res')
          (> res goal) (recur low (dec mid) probes)
          :else {:res mid})))))

(defn part-1 []
  (get-ore 1))

(defn part-2 []
  (let [{:keys [res probes]} (binary-search get-ore ore-supply 0 ore-supply)]
    (or res
        (->> probes
             (sort-by :res)
             (take-while #(< (:res %) ore-supply))
             last
             :probe))))
