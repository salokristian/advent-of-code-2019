(ns intcode-computer
  (:require [clojure.core.async :refer [close! thread chan <!! >!!]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn instruction-data [opcode]
  (case opcode
    1 {:size              4
       :has-output-param? true
       :update-program    (fn [{:keys [program], [p1 p2 out] :params}]
                            (assoc program out (str (+ p1 p2))))}
    2 {:size              4
       :has-output-param? true
       :update-program    (fn [{:keys [program], [p1 p2 out] :params}]
                            (assoc program out (str (* p1 p2))))}
    3 {:size              2
       :has-output-param? true
       :update-program    (fn [{:keys [program input-chan], [out] :params}]
                            (assoc program out (str (<!! input-chan))))}
    4 {:size         2
       :send-outputs (fn [{:keys [output-chan], [p1] :params}]
                       (>!! output-chan p1))}
    5 {:size           3
       :update-counter (fn [{:keys [counter size], [p1 p2] :params}]
                         (if (not (zero? p1)) p2 (+ counter size)))}
    6 {:size           3
       :update-counter (fn [{:keys [counter size], [p1 p2] :params}]
                         (if (zero? p1) p2 (+ counter size)))}
    7 {:size              4
       :has-output-param? true
       :update-program    (fn [{:keys [program], [p1 p2 out] :params}]
                            (assoc program out (if (< p1 p2) "1" "0")))}
    8 {:size              4
       :has-output-param? true
       :update-program    (fn [{:keys [program], [p1 p2 out] :params}]
                            (assoc program out (if (= p1 p2) "1" "0")))}
    9 {:size                 2
       :update-relative-base (fn [{:keys [relative-base], [p1] :params}]
                               (+ relative-base p1))}
    99 {:size 1}))

(defn parse-param [{:keys [program relative-base]} param param-mode]
  (case param-mode
    0 (edn/read-string (get program param))
    1 param
    2 (edn/read-string (get program (+ relative-base param)))))

(defn parse-out-param [{:keys [relative-base]} param param-mode]
  (case param-mode
    0 param
    1 param
    2 (+ relative-base param)))

(defn parse-params [context params-raw param-modes has-output-param?]
  (if has-output-param?
    (conj (mapv (partial parse-param context) (butlast params-raw) (butlast param-modes))
          (parse-out-param context (last params-raw) (last param-modes)))
    (mapv (partial parse-param context) params-raw param-modes)))

(defn param-modes+opcode [val]
  (let [[[_ param-modes opcode]] (re-seq #"(^\d*(?=\d{2})|^)(\d{1,2})" val)
        param-modes (->> param-modes str/reverse (map (comp edn/read-string str)))]
    {:opcode      (Integer/parseInt opcode)
     :param-modes (lazy-cat param-modes (repeat 0))}))

(defn parse-instruction [{:keys [program counter] :as context}]
  (let [{:keys [param-modes opcode]} (param-modes+opcode (get program counter))
        {:keys [size has-output-param?]} (instruction-data opcode)
        params-raw (map edn/read-string (subvec program (inc counter) (+ counter size)))
        param-modes (take (dec size) param-modes)]
    {:opcode opcode
     :size   size
     :params (parse-params context params-raw param-modes has-output-param?)}))

(defn execute-instruction
  [{:keys [program counter relative-base] :as context}
   {:keys [opcode params size] :as instruction}]
  (let [{:keys [update-program update-counter update-relative-base send-outputs]
         :or   {update-program       (constantly program)
                update-counter       (constantly (+ counter size))
                update-relative-base (constantly relative-base)
                send-outputs         (constantly nil)}} (instruction-data opcode)
        f-params (merge context {:params params, :size size})]
    (send-outputs f-params)
    (merge context {:program       (update-program f-params)
                    :relative-base (update-relative-base f-params)
                    :counter       (update-counter f-params)})))

(defn halt [{:keys [program input-chan output-chan]}]
  (close! input-chan)
  (close! output-chan)
  program)

(defn execute
  [context]
  (let [{:keys [opcode] :as instruction} (parse-instruction context)]
    (if (not= opcode 99)
      (recur (execute-instruction context instruction))
      (halt context))))

(defn add-memory [program]
  (->> (repeat "0")
       (take 10000)                                         ; Modern memory management;)
       (into program)))

(defn run [{:keys [input-size output-size program]}]
  (let [input-chan (chan input-size)
        output-chan (chan output-size)
        program-chan (thread (execute {:counter       0
                                       :relative-base 0
                                       :program       (add-memory program)
                                       :input-chan    input-chan
                                       :output-chan   output-chan}))]
    {:input-chan   input-chan
     :output-chan  output-chan
     :program-chan program-chan}))

(defn parse [s]
  (str/split s #","))
