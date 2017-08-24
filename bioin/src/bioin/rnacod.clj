(ns bioin.rnccod
  (:require [clojure.test :as t]
            [clojure.math.combinatorics :as combo]
            [incanter.core :as ic]
            [bioin.core :as c]))

(def rna-codon-table
["UUU" "F"      "CUU" "L"      "AUU" "I"     "GUU" "V" "UUC" "F"     "CUC" "L"     "AUC" "I"     "GUC" "V" "UUA" "L"     "CUA" "L"     "AUA" "I"     "GUA" "V " "UUG" "L"     "CUG" "L"     "AUG" "M"     "GUG" "V"
"UCU" "S"     "CCU" "P"     "ACU" "T"     "GCU" "A" "UCC" "S"     "CCC" "P"     "ACC" "T"     "GCC" "A" "UCA" "S"     "CCA" "P"     "ACA" "T"     "GCA" "A" "UCG" "S"     "CCG" "P"     "ACG" "T"     "GCG" "A"
"UAU" "Y"     "CAU" "H"     "AAU" "N"     "GAU" "D" "UAC" "Y"     "CAC" "H"     "AAC" "N"     "GAC" "D" "UAA" "Stop"   "CAA" "Q"     "AAA" "K"     "GAA" "E" "UAG" "Stop"   "CAG" "Q"     "AAG" "K"     "GAG" "E"
"UGU" "C"      "CGU" "R"     "AGU" "S"     "GGU" "G" "UGC" "C"      "CGC" "R"     "AGC" "S"     "GGC" "G" "UGA" "Stop"   "CGA" "R"     "AGA" "R"     "GGA" "G" "UGG" "W"     "CGG" "R"     "AGG" "R"     "GGG" "G"])

(def rna-cod-map
 (apply merge (map (fn[[k v]] {k v}) (partition 2 (map keyword rna-codon-table)))))

(t/with-test
  (defn translate-rna-into-protein
    [ins]
    (let [inp (->> (.split ins "")
                   (partition 3)
                   (map (partial clojure.string/join ""))
                   (map keyword)
                   (map rna-cod-map)
                   (take-while #(not= :Stop %))
                   (map name)
                   (clojure.string/join ""))]
      inp))
  (let [inp "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" ]
    (t/is (= "MAMAPRTEINSTRING" (translate-rna-into-protein inp)))))

(translate-rna-into-protein (slurp "/home/kiran/Downloads/rosalind_prot (1).txt"))
(count (slurp "/home/kiran/Downloads/rosalind_prot (1).txt"))

(vals rna-cod-map)
