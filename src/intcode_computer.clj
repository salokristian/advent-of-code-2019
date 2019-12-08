(ns intcode-computer
  (:require [clojure.core.async :refer [close! thread chan <!! >!!]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn instruction-data [opcode]
  (case opcode
    1 {:size           4
       :update-program (fn [{:keys [program params out]}]
                         (assoc program out (str (+ (first params) (second params)))))}
    2 {:size           4
       :update-program (fn [{:keys [program params out]}]
                         (assoc program out (str (* (first params) (second params)))))}
    3 {:size           2
       :update-program (fn [{:keys [program out input-chan]}]
                         (assoc program out (str (<!! input-chan))))}
    4 {:size         2
       :send-outputs (fn [{:keys [params output-chan]}]
                       (>!! output-chan (first params)))}
    5 {:size           3
       :update-counter (fn [{:keys [params counter size]}]
                         (if (not (zero? (first params))) (second params) (+ counter size)))}
    6 {:size           3
       :update-counter (fn [{:keys [params counter size]}]
                         (if (zero? (first params)) (second params) (+ counter size)))}
    7 {:size           4
       :update-program (fn [{:keys [program out params]}]
                         (assoc program out (if (< (first params) (second params)) "1" "0")))}
    8 {:size           4
       :update-program (fn [{:keys [program out params]}]
                         (assoc program out (if (= (first params) (second params)) "1" "0")))}
    99 {:size 1}))

(defn parse-param [program param param-mode]
  (case param-mode
    0 (edn/read-string (get program param))
    1 param))

(defn param-modes+opcode [val]
  (let [[[_ param-modes opcode]] (re-seq #"(^\d*(?=\d{2})|^)(\d{1,2})" val)
        param-modes (->> param-modes str/reverse (map (comp edn/read-string str)))]
    {:opcode      (Integer/parseInt opcode)
     :param-modes (lazy-cat param-modes (repeat 0))}))

(defn parse-instruction [program program-counter]
  (let [{:keys [param-modes opcode]} (param-modes+opcode (get program program-counter))
        {:keys [size]} (instruction-data opcode)
        params (map edn/read-string (subvec program (inc program-counter) (+ program-counter size)))]
    {:opcode     opcode
     :size       size
     :param-data (take (dec size) (map vector params param-modes))}))

(defn execute-instruction
  [{:keys [program counter] :as context}
   {:keys [opcode param-data size] :as instruction}]
  (let [params (map (partial apply parse-param program) param-data)
        out (-> param-data last first)
        {:keys [update-program update-counter send-outputs]
         :or   {update-program (constantly program)
                update-counter (constantly (+ counter size))
                send-outputs   (constantly nil)}} (instruction-data opcode)
        f-params (merge context {:params params, :out out, :size size})]
    (send-outputs f-params)
    (merge context {:program (update-program f-params)
                    :counter (update-counter f-params)})))

(defn halt [{:keys [program input-chan output-chan]}]
  (close! input-chan)
  (close! output-chan)
  program)

(defn execute
  [{:keys [program counter]
    :as   context}]
  (let [{:keys [opcode] :as instruction} (parse-instruction program counter)]
    (if (not= opcode 99)
      (execute (execute-instruction context instruction))
      (halt context))))


(defn run [{:keys [input-size output-size program]}]
  (let [input-chan (chan input-size)
        output-chan (chan output-size)
        program-chan (thread (execute {:counter     0
                                       :program     program
                                       :input-chan  input-chan
                                       :output-chan output-chan}))]
    {:input-chan   input-chan
     :output-chan  output-chan
     :program-chan program-chan}))

(defn parse [s]
  (str/split s #","))
