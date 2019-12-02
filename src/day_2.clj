(ns day-2
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private instr-size 4)

(defn- idx->in [idx]
  (when idx [(quot idx instr-size) (mod idx instr-size)]))

(defn opcode->operation [opcode]
  (case opcode
    1 +
    2 *
    99 ::KILL))

(defn parse-instruction [program program-counter]
  (let [[opcode p1 p2 p3] (get program program-counter)
        instruction [(opcode->operation opcode) (idx->in p1) (idx->in p2) (idx->in p3)]]
    (into [] (filter some? instruction))))

(defn execute [program program-counter]
  (let [[operation op-1-in op-2-in res-in] (parse-instruction program program-counter)]
    (if (not= operation ::KILL)
      (let [result (operation (get-in program op-1-in)
                              (get-in program op-2-in))
            program' (assoc-in program res-in result)]
        (execute program' (inc program-counter)))
      program)))

(defn parse [s]
  (->> (str/split s #",")
       (map edn/read-string)
       (partition-all instr-size)
       (mapv (partial mapv identity))))

(def ^:private program
  (first (common/parse-file parse "day_2.txt")))

(defn part-1 []
  (execute program 0))


(def ^:private inputs
  (for [noun (range 100)
        verb (range 100)]
    [noun verb]))

(defn create-program [[noun verb]]
  (-> program
      (assoc-in [0 1] noun)
      (assoc-in [0 2] verb)))

(defn has-output [val program]
  (when (= (first (first program)) val)
    program))

(defn part-2 []
  (->> inputs
       (map create-program)
       (map #(execute % 0))
       (some (partial has-output 19690720))))
