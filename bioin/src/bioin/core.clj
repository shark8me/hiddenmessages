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
(defn par-to-num [pat]
  (let [m (zipmap ["a" "c" "g" "t"] (iterate inc 0))
        patvec (.split pat "")]
    (apply +
           (map #(* %1 %2)
       (reverse (map m (.split pat "")))
         (into [1] (take (count patvec) (iterate #(* % 4) 4)))))))

(par-to-num "gt")
(par-to-num "agt")
;(par-to-num "atgcaa")

(def fin (slurp "/home/kiran/Downloads/dataset_3010_2.txt"))
(par-to-num (.toLowerCase (first (.split fin "\n"))))

(defn num-to-pat [xin,k]
  (let [m (zipmap (iterate inc 0) ["a" "c" "g" "t"])]
(loop [x xin
       acc []]
  (let [[q r] (map #(% x 4) [quot rem])
        nacc (conj acc (if (not= 0 r) r 0))]
    (if (= k (count acc))
      (clojure.string/join (map m (reverse acc)))
      ;(reverse (take k nacc))
      (recur q nacc))))))

(pat-to-num 11 2)
(pat-to-num 912 6)
(pat-to-num 5437 8)
(pat-to-num 0 2)
(pat-to-num 45 4)

(def fin (slurp "/home/kiran/Downloads/dataset_3010_4.txt"))
fin
(let [[x y] (map #(Integer/parseInt %) (.split fin "\n"))]
  (pat-to-num x y))
(pat-to-num 6336 9)
(pat-to-num 8359 8)
(pat-to-num 8359 8)
(defn computefreq [pat,k]
(let [n (int (Math/pow 4 k))
      arr (map #(pat-to-num % k) (range n))
      kmap (apply merge (map #(assoc {} % 0) arr))
      mapwithscore (reduce (fn[acc k] (update-in acc [k] inc))
        kmap (map clojure.string/join (partition k 1 (.split pat ""))))]
   (clojure.string/join " " (map mapwithscore arr) )
  ))

(computefreq "acgcggctctgaaa" 2)
(def k1 (computefreq "abc" 2))

(reduce (fn[acc k] (update-in acc [k] inc))
        k1 (map clojure.string/join (partition 2 1 (.split "acgcggctctgaaa" ""))))

(def inp "cactcagtgctccgtactcgactgtacaaggacgagcactagatacctacgtaacatcccgtggtccacagttaacacctgggcaacctcgacgcgggggaccacgaccaagtgtcacgtggcatatcatccagagtttgatattagggcaatggtcaggagcaagctaggccgagtctctacttgagacccacacgctacaaggatttgcccaagctgaatgtggcagggtgtgacacgacacggcgcgggtcgtcgcttctaacaaccatgcaaacttaggtggcagttgttaagttggctttaatcgagaagtgggatcttagttcaacaggagtggctcccaaagaagggccagccctaggcctcctattatgatagaaacgttaggtggtgacagttggtctagtactgttgctcagacttgacttcaataatcttcatttacattcaagagagaaaaggacgtgcatcggtccattagtccgagtactatacgttgaaaggagcaccgtggcgggtcttccagggttggcattacgtagatgggtaaagctgcatcgcaggattgtatatctaagcacttccgtcacccagctgtctaggacttgcgcatcaaccataccaggccctcacagatgctatatgtactgctcaatgacaggggttcgcctcggtatgcgtccccgcgaggtatcaatatcgcgagacgtagcgtgatgttgtggctccgcaacg")
(computefreq inp 7)


(let [st (.split "agt" "")
      m (zipmap ["a" "c" "g" "t"] (iterate inc 0))]
(loop [f1 st
       acc []]
   (if (empty? f1)
     (let [k (reverse acc)]
      ; (+ (first k) (apply + (map #(* %1 %2) (rest k) (iterate #(* % 4) 4)))))
      ; (map #(vector %1 %2) (rest k) (iterate #(* % 4) 4)))
     acc)
     (recur (butlast f1) (conj acc (m (last f1)))))))


(defn pat-to-num
(let [st (.split "agt" "")
      m (zipmap ["a" "c" "g" "t"] (iterate inc 0))
      k (reverse (map m st))]
  (+ (first k) (apply + (map * (rest k) (iterate #(* % 4) 4)))))

