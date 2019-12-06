(ns common
  (:require [clojure.java.io :as io]))

(defn parse-file [parse-line name]
  (with-open [rdr (-> name io/resource io/reader)]
    (doall (map parse-line (line-seq rdr)))))

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn find-pred [pred x]
  (some #(if (pred %) % nil) x))
