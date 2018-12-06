; (def inputNumbers [7 7 -2 -7 -4])
(def inputNumbers
  (->>
   (slurp "inputs/day1.txt")
   (clojure.string/split-lines)
   (mapv #(Integer/parseInt %))))

(def answer1
  (reduce + inputNumbers))

(def answer2
  (let [frequencyChanges (reductions + (cycle inputNumbers))]
    (loop [[first & rest] frequencyChanges
           seen #{0}]
      (if (contains? seen first)
        first
        (recur rest (conj seen first))))))

(println answer1)
(println answer2)