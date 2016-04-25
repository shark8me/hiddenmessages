(ns bioin.chap2
  (:require [clojure.test :as t]
            [clojure.math.combinatorics :as combo]
            [bioin.core :as c]))

(t/with-test
  (defn skew
    "chap 2.3 step 4"
    [haystack]
    (let [m {"c" -1 "g" 1}
          s (map #(get m % 0) (c/split-tl haystack))]
      (reduce (fn [acc x] (conj acc (+ x (last acc)))) [0] s )))
  (let [haystack "CATGGGCATCGGCCATACGCC"]
    (t/is (= '(0 -1 -1 -1 0 1 2 1 1 1 0 1 2 1 0 0 0 0 -1 0 -1 -2)
             (skew haystack)))))


(t/with-test
  (defn index-of-min
    [haystack]
    (let [sku (skew haystack)
          kmin (apply min sku)]
      (remove nil? (map #(if (= kmin %1) %2) sku (iterate inc 0)))))
  (let [haystack "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"]
    (t/is (= '(11 24) (skew haystack)))))

(comment
  ;problem in chapter 2.3, step 7
(time
(let [hays (slurp "/home/kiran/Downloads/dataset_7_6 (1).txt")]
  (index-of-min hays))))

(comment
(let [hays (slurp "/home/kiran/Documents/minimum_skew_data.txt")
      inp (second (.split hays "\n"))]
  (index-of-min inp)))

(t/with-test
  (defn hamming-distance
    [s1 s2 ]
    (count (filter (fn [[x y]] (not= x y))
                   (partition 2 (apply interleave (map c/split-tl [s1 s2]))))))
  (let [s1 "GGGCCGTTGGT"
        s2 "GGACCGTTGAC"]
    (t/is (= 3 (hamming-distance s1 s2)))))

(comment
  ;chapter 2.4 step 3
  (let [hays (.split (slurp "/home/kiran/Downloads/dataset_9_3.txt") "\n")
        [s1 s2] (map #(% hays) [first second])]
    (hamming-distance s1 s2)))

(t/with-test
  (defn approx-match
    "problem in chap 2.4 step 4"
    [needle haystack  k ]
    (let [ ne (clojure.string/join (c/split-tl needle))
           ha (c/part-tl haystack (count needle))
           hamfn (fn [ k v] (count (remove false? (map #(= %1 %2) k v))))
           hamdists (map (partial hamming-distance ne) ha)]
      (remove nil? (map #(if (>= k %1) %2) hamdists (iterate inc 0)))))
  (let [needle "ATTCTGGA"
        haystack "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
        k 3]
    (t/is (= '(6 7 26 27)
             (approx-match needle haystack k)))))

(comment
  ;chapter 2.4 step 4
  (let [hays (.split (slurp "/home/kiran/Downloads/dataset_9_4 (1).txt") "\n")
        [ne ha] (map #(% hays) [first second])
        k (nth hays 2)
    res (approx-match ne ha (Integer/parseInt k))]
    (spit "/tmp/res" (clojure.string/join " " res))))

(t/with-test
  (defn count-d
    "2.4 step 5 "
    [ haystack needle k]
    (count (approx-match needle haystack k)))
  (let [haystack "AACAAGCTGATAAACATTTAAAGAG" needle "AAAAA" k 1]
    (t/is (= 4 (count (approx-match needle haystack k))))))

(comment
  ;2.4 step 5
(let [ haystack "AACAAGCTGATAAACATTTAAAGAG" nee "AAAAA" k 2]
  (count-d haystack nee k)))

(comment
  ;2.4 step 6
  (let [hays (.split (slurp "/home/kiran/Downloads/dataset_9_6.txt") "\n")
        [ne ha] (map #(% hays) [first second])
        k (nth hays 2)]
    (count-d ha ne (Integer/parseInt k))))

(t/with-test
  (defn insert-at
    "given string hays, returns copies of hays with one element of fill-with inserted at
    ith position"
    [hays ith ]
    (let [as #{"a" "c" "g" "t"}
          sina (vec (c/split-tl hays))
          fill-with (disj as (nth sina ith))
          pref (subvec sina 0 ith)
          suffix (subvec sina (inc ith))]
      (vec (for [i fill-with]
        (.toUpperCase (clojure.string/join (into (conj pref i) suffix)))))))
  (t/is (= '["ACA" "ACT" "ACC"] (insert-at "acg" 2))))

(t/with-test
  (defn d-neighbourhood
    "CS-generating neighbourhood of string-step 3"
    [ha k]
    (if (empty? k)
      ha
      (let [k1 (first k)
            ha1 (mapcat #(insert-at % k1) ha)]
        (recur ha1 (subvec k 1)))))
  (let [ha ["ACG"]
        k [ 1 2 ]
        res '("AAA" "AAT" "AAC" "ATA" "ATT" "ATC" "AGA" "AGT" "AGC")]
    (t/is (= res (d-neighbourhood ha k)))))

;(let [inp "GGCCCAGAG" k 3]
(defn get-all-d-neighbourhoods
  [inp k]
  (let [pos (combo/combinations (range (count inp)) k)]
    (mapcat (partial d-neighbourhood (vector inp)) (map vec pos))))

(comment
  ;CS-generating neighbourhood of string-step 4"
  (spit "/tmp/op" (clojure.string/join "\n" (get-all-d-neighbourhoods "TTATCTACGA" 2))))

(t/with-test
  (defn freqword-w-mismatch
    [h k misma]
    (let [parted (c/part-tl h k)
          freq-parted (frequencies (map #(.toUpperCase %) parted))
          neighbour-to-src (->> freq-parted
                                keys
                                (map #(zipmap (get-all-d-neighbourhoods % misma) (repeat 1)))
                                (apply (partial merge-with +))
                                (sort-by second (fn [ x y ](> x y)))
                                (partition-by second)
                                first
                                #_(map first))]
      neighbour-to-src))
  (let [h "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        ;"AACAAGCTGATAAACATTTAAAGAG"
        k 4 misma 1 res ["AAAAA"]]
    (freqword-w-mismatch h k misma)
    ;this doesn't match with the test case
    #_(t/is (= res (freqword-w-mismatch h k misma)))))
(comment
(let [h "CACAGTAGGCGCCGGCACACACAGCCCCGGGCCCCGGGCCGCCCCGGGCCGGCGGCCGCCGGCGCCGGCACACCGGCACAGCCGTACCGGCACAGTAGTACCGGCCGGCCGGCACACCGGCACACCGGGTACACACCGGGGCGCACACACAGGCGGGCGCCGGGCCCCGGGCCGTACCGGGCCGCCGGCGGCCCACAGGCGCCGGCACAGTACCGGCACACACAGTAGCCCACACACAGGCGGGCGGTAGCCGGCGCACACACACACAGTAGGCGCACAGCCGCCCACACACACCGGCCGGCCGGCACAGGCGGGCGGGCGCACACACACCGGCACAGTAGTAGGCGGCCGGCGCACAGCC"
      k 10 misma 2]
  (freqword-w-mismatch h k misma)))


(comment
(apply (partial merge-with + )(map (fn[[k v]] (let [nm (neighbour-to-src k)]
                       {nm v})) neighbour-freq))
(apply (partial merge-with inc)
         (map (fn[[k v]] {k v})
       (filter (fn[[k v]] (> v 0))
          (reduce (fn[acc i] (if (nil? (acc i)) acc
                               (update-in acc [i] inc)))
          nfreq-parted (map #(.toUpperCase %) parted)))))
)
