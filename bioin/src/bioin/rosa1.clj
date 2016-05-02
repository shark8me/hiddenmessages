(ns bioin.rosa1
  (:require [clojure.test :as t]
            [clojure.math.combinatorics :as combo]
            [incanter.core :as ic]
            [bioin.core :as c]))

(comment
;counting dna nucleotides
(let [samp "TAGAGAATTCGAACCAGAATAATGCCGTACGCAATTATTCTGCCTTGGGTCTGGAAATAGGTACTGCACACAGTGATTTGGCAACGACCTCAAGCCACGAGCGGTAGCCCTAAAAAAGGCATTCATGCTCATTACAAGTCGTATACCCACCGGCTTCATAGATAAACGCGGACCGTCTCTATTTTCCGAAGCGTAAGTGCTAATGCCTGGTACTAAGTGCAGGGTAAAGTGGCCTACTAGCCTGAGGAGCCACATCGAGATGTTATCGGAAGGGCGTTTTTACTTTAGAAGGCACCATACGTCTTGACTGAGGAAGTGGACGACAGCACCGGTGACCACTATGATTTAAGACCACTACACTTCTCGAGTTTACCTTTGACAGTCTGGCAACAGAGATGAAGAACTTGAACCCCTTCGCGAGCCTCTTCGGACTGCCGGCACTCGGCAATCGCGACGGCTGGTTTAGTTTATACCTGACGCCGGACTGGTCGACCCCTGATGGGCTAGTCCACCGCAAAATCCTTCCTGACCGGTAGCGCCGATAATAAAAATACCTGGTGAGGAATGACCTAAAACAGGCGAGCCTGCTGGGCTATGACATCTTTCCATCTTTACCGTAGATCGATATAATCACACAGCGAAAGATGTATTTGCAATTGGTAACTCTCGTCGTAGTTAGTGAATAGTCTGATTCGTCGACCCTGATGGAACTGCACCGAATGTGGTAATCTTAGATAGGGTTGAGTGTCTTTGTTCGGGAATTGCGCACGACGGCTAAGTCATGGCCTGGCGTGTGCTGGTCTATTTGCACGCACACTCACTCCACTTGTCACCTTTCGGCTCT"]
  (apply (partial merge-with +) (map #(assoc {} % 1) (c/split-tl samp))))
;transcribe dna to rna
(let [samp "TTAATATGTCGGCGGTTCAATCACACAAGACTAGACCTTTGCGCATTAAGTGACGGTGCCCTTAGTCAGCTCACGATGTGCTATAAATGTTGGAGATAACGTATTAAGTTCTGAGTCGAGAGCTAGTACTCCGGAGGGACGGGACGCTCCCTGGGGCGGCAGTTGGTTGCCGCGACTTGCGCCTTAGCCAGGCACGGAAAATGGAGAGCGCCTCGGATAGATGGAGTGCAGGCATCATCGTGAGTGGGCTAGGCGTTCAGGGACGAATTTCGGTTGACCGCGACTCTTACCATCACCAGTTGGTATTTCTCGCGGTACTTATCCTGTAGATCGGGCGCTTGGTGAGATTAGCAATGTCAGTCGTAATCCGATATGACCGCAAAGAGCGCATGAAAGCTACACTTCCGCCCTCGGGCCGTCCGTATTATCAATTTGTGCGCGGGCCCACCTGCTCATCTCTCAGCCATGGTCTACTGCAGTGGAAGGCTGCGTCCCGCTCTCTCGGGCTACATGCGATCTAGTGCGCGTACTGTTTATTACTGTGATCGACTTAGGGCAATAATGGCAAGCAAGGAAAAAGTTAGGAACAGGATGGACTGTTCTGGCGTAACAACGTGACATAAAACAAGGGATCTCTCTGCAGAGTACTTTTCATAAGTCTGACGCAGCGGATACATTGTTATTCCCAAATATATAGTGGGACCTTATGGATTAGCGCGGCCTTGATATCTCTATGGTGTGGACGTGTATTGATGAAGGCCTGCGGTCGATACGGACCAGCCAACCACAAGGAGTAGATTTCATTCGCGCTCGAGTACAGTACATTCGATTCACAACTAATTAGATGTTAGTATTTGCATCTTCTCAGTCGATCACGACGGCATGAATCGAAGCTATATCCTATCCAATCTGCT"
      ]
  (.toUpperCase (clojure.string/join (map #(if (= "t" %) "u" %) (c/split-tl samp)))))

(let [samp "ACCTCACGCTTGAACCAATGCCTTGACAAGTTTTATTATTGGGGCCATCCCTATAACTAGTATCTGCCTTGAATGAATTTAATGCGACTCCCCAGTAAGAACTCGGAGAGGACCGCCATATGCTGAATGTAGAGTTGGGGAAATAGTTAATGTACTAAAGGAGTACGATGTACAATAAGATAGGACCAATGGGATATGGACCACTATTACTAGGAAAGGAGTTTACTGCGTGCCAAATTACTCACGACCGACCGGCTACGCGTAGCGACGACGACCCGGAGAACTTGGCATGGGAGTCCAGCGAACTTTAGAAAACCTCCGATGCGCAGTGTAACATACCTCACACTTCGTCCTGCTTGTCTATGGCGTGTAAGTATCGGATGCGATATCGGATCGTAGAACAAGGGTAACTTAAAATGCATGCCGCGTGTCCGCGGTCTATACCCCGCATCGCGTAGAAGAATCGGAGAGGAGCTCTGAGATGGACGAAATGATTGACGGCGTGATACGCCCACCTGGACTTGTGTTAAAGACCCGTGGAAAGCTGTACACTCGGTCTGTACTCTTCGCTTCGCAGTCGTACTAAGAAAACTTAGCCATGGGGAGCCCACTCGCCTGTGTAGGATATGGTAGCGCCCACCAGCCACACGTAATGGAATGAGTGATTCGCGTCAAATGCAGGAAAAAGGGGGTGACAAAAACTTCTAACGATGAGCACGGAGGGTCTCACCGCTTAACCACCATTTCACGTATGGAGGGGCGGGTTTCTAGCTGCTCATCTATCAATTGTTTGAAAAGGTGAGGCA"]
  (c/reverse-complement samp))
)

(t/with-test
  (defn fibo
    [ini gen]
    (let [ init [ ini ini]
           ifn (fn [x] (let [[i1 i2] (map #(x %) [0 1])]
                         (into [(+ i1 i2)] x)))]
      (->> (iterate ifn init)
           (take gen)
           last
           first)))
  (t/is (= 8 (fibo 1 5)))
  (t/is (= 13 (fibo 1 6))))

(t/with-test
  (defn fibonk
    "fibonacci with k pairs produced per generation"
    [ini gen k ]
    (let [ init [ ini ini]
           ffn (fn [i1 i2] (+ (- i1 i2) (* (inc k) i2)))
           ifn (fn [x] (let [[i1 i2] (map #(x %) [0 1])]
                         (into [(ffn i1 i2)] x)))]
      (->> (iterate ifn init)
           (take (dec gen))
           last
           first)))
  (let [k 3]
    (t/is (= 7 (fibonk 1 4 k)))
    (t/is (= 19 (fibonk 1 5 k)))))

(t/with-test
  (defn fibomort
    "fibonacci with mortality, of k generations "
    [init gen k ]
    (let [ ffn (fn [i1 i2] (+ i1 i2))
           ifn (fn [x] (let [[i1 i2] (map #(x %) [1 2])]
                         (into [(ffn i1 i2)] x)))]
      (->> (iterate ifn init)
           (take-while #(<= (count %) gen))
           last
           first
           )))
    (t/is (= 2 (fibomort [2 1 1] 4 3)))
    (t/is (= 3 (fibomort [2 1 1] 5 3))))

; 1 1 2 2 3 4 5 7 9 12
;(fibomort [2 1 1] 87 3)
;1 1
;2 {3 1 1 1}
;2 {2 1 1 1}
;3 {3 1 1 2 2 1}
(defn incr
  [age [k v]]
  (cond
    (= k 1) {(inc k) v}
    (>= k age) {1 v}
    :else {(inc k) v 1 v}))

(defn iterfn
  [age x]
  (->> x
       (map (partial incr age))
       (apply (partial merge-with +))))

(t/with-test
  (defn getgen
    [gen age]
    (->> (iterate (partial iterfn age) {3 (BigInteger. "1") 1 (BigInteger. "1")})
         (take (- gen 2))
         (map (comp (partial apply +) vals))
         last
         ))
  (t/is (= 9.0 (getgen 9 3)))
  (t/is (= 16.0 (getgen 11 3))))
;(getgen 91 18)

;(spit "/tmp/ro" (clojure.string/join "\n" (map #(clojure.string/join "" %) (combo/selections ["B" "Z" "U" "I" "T"] 4 ))))
(defn compute-gc-content
  "compute GC as percentage "
  [infile]
  (let [ s (.split (slurp infile) ">Rosalind_")
         perce (fn[x] (let [spl (.split x "") c (count (filter #(some (fn[j] (.equalsIgnoreCase j %)) ["G" "C"]) spl ))
                            len (count spl)]
                        (float (/ c len))))
         fnx (fn[x] (let [ s1 (.split x "\n")
                           h (first s1)
                           r (clojure.string/join "" (rest s1))]
                      [h (perce r)]))
         [f se] (apply (partial max-key second) (map fnx s))
         fres (str ">Rosalind_" f) ]
    (spit "/tmp/op" (clojure.string/join "\n" [fres (* 100 se)]))
    (map fnx s)))
;(compute-gc-content "/home/kiran/Downloads/rosalind_gc (1).txt")
;(compute-gc-content "/tmp/ro")

(t/with-test
  (defn get-dominant-prob
    "returns the probability of a dominant allele"
    [[i1 i2]]
    (let [m {:k [:Y :Y] :m [:Y :y] :n [:y :y]}
          cartprod (combo/cartesian-product (m i1) (m i2))]
      (float (/ (count (filter (partial some #{:Y}) cartprod)) (count cartprod)))))
  (t/is (= 1 (int (get-dominant-prob [:k :k])))))
(comment
(combo/combinations [:k :k :m :m :n :n] 2)
(get-dominant-prob [:m :m])
(->> (combo/combinations [:k :k :m :m :n :n] 2)
     (map get-dominant-prob)
     ;(map #(/ % 6))
     #_(apply +))
)

(t/with-test
  (defn num-instances
    "n choose k variant"
    [m [i1 i2]]
    (if (= i1 i2)
      (int (ic/choose (m i1) 2))
      (* (m i1) (m i2))))
  (let [m { :k 2 :m 2 :n 2}]
    (t/is (= 4 (num-instances m [:m :k])))
    (t/is (= 1 (num-instances m [:k :k])))))

(t/with-test
  (defn mendels-first-law
    "solving mendel's first law"
    [m]
    (let [nc (mapcat (fn[[k v ]] (repeat v k )) m)
          combs (combo/combinations nc 2)
          instcount (map (partial num-instances m) combs)
          totinst (ic/choose (apply + (vals m)) 2)
          probs (map get-dominant-prob combs)]
      (apply + (map * probs (map #(/ % totinst) instcount)))))
  (let [m { :k 17 :m 25 :n 26}]
    (t/is (< 0.00001 (ic/abs (- 0.6817 (mendels-first-law m)))))))
