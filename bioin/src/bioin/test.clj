(ns bioin.test
  (:require [incanter]))

(defn randx
  [n]
  (for [_ (range n)]
    (let [samp (take 10 (repeatedly (partial rand 1)))]
      (- 0.5 (/ (apply + samp) (count samp))))))

(def orig (randx 10))
orig

(def sorted-orig (sort-by - orig))
sorted-orig

(defn get-index
  [h i]
  (first (filter #(not (nil? %)) (map #(if (= %1 i) %2) h (range 10)))))

(defn get-rank-order
  [inp]
  (let [indexfn (fn [h i] (first (filter #(not (nil? %)) (map #(if (= %1 i) %2) h (range 10)))))
        sorted-orig (sort-by - inp)]
    (map #(indexfn sorted-orig %) inp)))

(get-rank-order orig)
