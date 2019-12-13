(ns day-13
  (:require [clojure.core.async :as async :refer [<!! >!!]]
            [intcode-computer :as icmp]
            [common :as common]))

(def program-code (first (common/parse-file icmp/parse "day_13.txt")))
(def program (icmp/run {:program     (assoc program-code 0 "2")
                        :input-size  1
                        :output-size 3}))
(def tiles
  ;; Use a magic number to output the map layout from the program before starting to play
  (let [tile-output (<!! (async/into [] (async/take (* 3 756) (:output-chan program))))]
    (->> tile-output (partition 3) (map vec))))

(defn take-output [output-chan]
  (<!! (async/into [] (async/take 3 output-chan))))

(defn output->type [output]
  (cond
    (empty? output) nil
    (= [-1 0] (subvec output 0 2)) :score
    (= 3 (peek output)) :paddle
    (= 4 (peek output)) :ball))

(defn assoc-output [state output type]
  (let [val (case type
              :score (peek output)
              :paddle (subvec output 0 2)
              :ball (subvec output 0 2)
              nil)]
    (if (some? val)
      (assoc state type val)
      state)))

(defn update-state [state output-chan]
  (loop [state state]
    (let [output (take-output output-chan)
          output-type (output->type output)
          state' (assoc-output state output output-type)
          waits-input? (= output-type :ball)
          game-end? (empty? output)]
      (if (or waits-input? game-end?)
        {:state state'
         :end?  game-end?}
        (recur state')))))

(defn joystick [{[ball-x] :ball [paddle-x] :paddle}]
  (cond
    (= ball-x paddle-x) 0
    (< ball-x paddle-x) -1
    (> ball-x paddle-x) 1))

(defn play [{:keys [state output-chan input-chan]}]
  (loop [state state]
    (>!! input-chan (joystick state))
    (let [{state' :state, end? :end?} (update-state state output-chan)]
      (if end?
        state'
        (recur state')))))

(defn part-1 []
  (count (filter #(= 2 (peek %)) tiles)))

(defn part-2 []
  (play {:state       {:paddle (common/find-pred #(= 3 (peek %)) tiles)
                       :ball   (common/find-pred #(= 4 (peek %)) tiles)
                       :score  (peek (take-output (:output-chan program)))}
         :input-chan  (:input-chan program)
         :output-chan (:output-chan program)}))
