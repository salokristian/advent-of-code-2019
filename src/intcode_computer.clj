(ns intcode-computer
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn opcode->data [opcode]
  (case opcode
    1 {:op + :size 4 :output? true}
    2 {:op * :size 4 :output? true}
    3 {:op (constantly 1) :size 2 :output? true}
    4 {:op println :size 2 :output? false}
    99 {:op ::KILL :size 1 :output? false}))

(defn parse-param [program param param-mode]
  (case param-mode
    0 (edn/read-string (get program (edn/read-string param)))
    1 (edn/read-string param)))

; regex
(defn param-modes+opcode [val]
  (let [opcode-idx (- (count val) 2)
        [param-chars opcode-chars] (split-at opcode-idx val)
        param-op-codes (->> param-chars reverse (map str) (map edn/read-string))]
    (assoc
      (opcode->data (->> opcode-chars (map str) (apply str) edn/read-string))
      :param-modes (lazy-cat param-op-codes (repeat 0)))))

(defn parse-instruction [program program-counter]
  (let [{:keys [op size output? param-modes]} (param-modes+opcode (get program program-counter))
        params (subvec program (inc program-counter) (+ program-counter size))
        param-data (map vector params param-modes)]
    {:op   op
     :size size
     :in   (if output? (butlast param-data) param-data)
     :out  (when output? [(last params) 1])}))

(defn execute-instruction [program {:keys [op in out]}]
  (let [in' (map (partial apply parse-param program) in)
        result (str (apply op in'))]
    (if-not (nil? out)
      (assoc program (apply parse-param program out) result)
      program)))

(defn execute [program program-counter]
  (let [{:keys [op size] :as instruction} (parse-instruction program program-counter)]
    (if (not= op ::KILL)
      (execute (execute-instruction program instruction)
               (+ program-counter size))
      program)))

(defn parse [s]
  (str/split s #","))
