(ns bioin.core)

;(def tst (slurp "/home/kiran/src/mooc/hiddenmessages/"))

(let [geno "AAAACGTCGAAAAA"
      k 2
      l 4
      t 2
  imap (apply merge-with into
   (map #(assoc {} (clojure.string/join %1) [%2]) (partition k 1 (.split geno "")) (iterate inc 1)))]
  (filter (fn [[k1 v1]] (>= (count v1) 2)) imap)
  )

(reduce (fn[acc x] (let [l1 (peek acc)]
                     (if (nil? l1)
                       (conj acc x)
                       (do
                         (println " acc " acc " x " x)
                         (if (> (- x l1) 2)
                         (conj acc x) acc)))))
           [] [1 2 3 10 11 12 13])

(defn remove-overlaps [inp k]
(reduce (fn[acc x] (let [l1 (peek acc)]
                     (if (or (nil? l1) (> (- x l1) k))
                         (conj acc x) acc)))
           [] inp))

(remove-overlaps [1 2 3 10 11 12 13] 2)


(defn isvalid [lst k l t]
  )

;;pattern to number
(let [m (zipmap ["a" "c" "g" "t"] (iterate inc 0))]
  (map #(vector %1 %2)
       (reverse (map m (.split "gt" ""))) (iterate #(* % %) 4)))

(take 3 (into [0] (iterate #(* % 4 ) 4)))
(into [0] (take 3 (iterate #(* % 4 ) 4)))
