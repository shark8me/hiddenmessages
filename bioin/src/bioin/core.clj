(ns bioin.core
  (:require [clojure.test :as t]))

;(def tst (slurp "/home/kiran/src/mooc/hiddenmessages/"))

(defn split-tl
  [haystack]
  (->>(.split haystack "")
      (map #(.toLowerCase %))))

(defn part-tl
  "convert haystack to lower case and partition into a sliding window
  that moves ahead by 1"
  [haystack k]
  (->> (split-tl haystack)
       (partition k 1 )
       (map clojure.string/join)))

(t/with-test
  (defn pattern-count
    "count occurrances of needle in haystack, with sliding window of length k"
    [needle haystack k ]
    (->> (part-tl haystack k)
        (filter #(= (.toLowerCase needle) %))
        count))
  (t/is (= 3 (pattern-count "ACTAT" "ACAACTATGCATACTATCGGGAACTATCCT" 5))))

(t/with-test
  (defn frequent-words
    "find frequent words in haystack, with length k. Returns a vector"
    [haystack k]
    (->>(part-tl haystack k)
        (map #(assoc {} % 1))
        (reduce (partial merge-with +))
        (reduce (fn [acc [k v]]
                  (if (empty? acc)
                    [v [k]]
                    (cond
                      (> v (acc 0)) [v [k]]
                      (= v (acc 0)) [v (conj (acc 1) k)]
                      :else acc))) [])
        second))
  (t/is (= ["ctat" "acta"]
           (frequent-words "CAACTATGCATACTATCGGGAACTATCCT" 4))))

(t/with-test
  (defn reverse-complement
    "Chapter 1.3 step 2. reverse complement of a DNA string"
    [inp]
    (let [rmap  (zipmap ["a" "t" "c" "g"] ["t" "a" "g" "c"])]
      (->> (split-tl inp)
           (map rmap)
           reverse
           (map #(.toUpperCase %))
           clojure.string/join)))
  (t/is (= "ACCGGGTTTT"
           (reverse-complement "AAAACCCGGT"))))

(t/with-test
  (defn index-finder
    "pattern matching problem, chapter 1.3 step 5"
    [needle haystack]
    (let [sneed (.toLowerCase needle)]
      (remove nil? (map #(if (= sneed %1) %2)
                        (part-tl haystack (count needle)) (iterate inc 0)))))
  (t/is (= '(1 3 9)
           (index-finder "ATAT" "GATATATGCATATACTT"))))

(t/with-test
  (defn find-kmer-wfreq
    "find kmer with freq greater than or equal to t"
    [k t haystack ]
    (->> (part-tl haystack k)
         frequencies
         (filter (fn[[k v]] (>= v t)))
         (map first)
         set))
  (t/is (= #{"cgaca"}
           (find-kmer-wfreq 5 4 "cggactcgacagatgtgaagaacgacaatgtgaagactcgacacgacaga" ))))

(t/with-test
  (defn clump-finder
    [haystack  k L t]
    (let [lseq (map clojure.string/join (partition L 1 (split-tl haystack)))]
      (->> lseq
           (map (partial find-kmer-wfreq k t))
           (reduce into #{}))))
  (let [haystack "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
        k 5 L 50 t 4]
    (t/is (= #{"cgaca" "gaaga"}
             (clump-finder haystack k L t)))))

;takes a long time
;(let [hays (slurp "/home/kiran/Documents/E-coli.txt")]
;  (clump-finder hays 9 500 3))

(t/with-test
  ;;pattern to number
  (defn pat-to-num
    "pattern to number Chapter 1.5, step 2 "
    [pat]
    (let [m (zipmap ["a" "c" "g" "t"] (iterate inc 0))
          patvec (split-tl pat)]
      (apply + (map * (reverse (map m patvec))
                    (into [1] (take (count patvec) (iterate #(* % 4) 4)))))))
  (t/is (= 11 (pat-to-num "GT")))
  (t/is (= 11 (pat-to-num "AGT")))
  (t/is (= 912 (pat-to-num "ATGCAA"))))

;(def fin (slurp "/home/kiran/Downloads/dataset_3010_2.txt"))
(t/with-test
  (defn num-to-pat
    "number to pattern, chapter 1.5, step 3"
    [xin,k]
    (let [m (zipmap (iterate inc 0) ["a" "c" "g" "t"])]
      (loop [x xin
             acc []]
        (let [[q r] (map #(% x 4) [quot rem])
              nacc (conj acc (if (not= 0 r) r 0))]
          (if (= k (count acc))
            (clojure.string/join (map m (reverse acc)))
            ;(reverse (take k nacc))
            (recur q nacc))))))
  (t/is (= "cccattc" (num-to-pat 5437 7)))
  (t/is (= "gt" (num-to-pat 11 2)))
  (t/is (= "agtc" (num-to-pat 45 4))))

(t/with-test
  (defn computing-frequencies
    "1.5 step 5 "
    [ haystack k]
    (let [pt (map pat-to-num (part-tl haystack k))
          freqmap (frequencies pt)]
      (map #(get freqmap % 0) (range (int (Math/pow 4 k))))))
  (let [ haystack "ACGCGGCTCTGAAA" k 2
         res [2 1 0 0 0 0 2 2 1 2 1 0 0 1 1 0]]
    (t/is (= res (computing-frequencies haystack k)))))

(comment
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

;(def fin (slurp "/home/kiran/Downloads/dataset_3010_4.txt"))
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
  (+ (first k) (apply + (map * (rest k) (iterate #(* % 4) 4))))))

)
