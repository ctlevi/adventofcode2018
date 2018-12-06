; (def inputNumbers [5 2 2 5 5 7])
(def inputNumbers
  (->>
   (slurp "inputs/day1-last-year.txt")
   (vec)
   (mapv str)
   (mapv #(Integer/parseInt %))))

(def zipped
  (->>
   (conj
    (subvec inputNumbers 1)
    (first inputNumbers))
   (mapv vector inputNumbers)))

(def answer1
  (->>
   (filterv (fn [[x y]] (= x y)) zipped)
   (mapv (fn [[x]] x))
   (reduce +)))

(def answer2
  (->>
   (split-at (/ (count inputNumbers) 2) inputNumbers)
   ; This performs a zip of the split done above
   (apply mapv vector)
   (filterv (fn [[x y]] (= x y)))
   ; Since the list wraps around, and we are considering the halfway around index of each element, we can assume
   ; that an index at i has a wrap around value of j and j has a wrap around value of i.
   ; x and y are the same, I could have done (* x 2) for instance
   (mapv (fn [[x y]] (+ x y)))
   (reduce +)))

(println answer1)
(println answer2)

; (println (solution inputNumbers))
; (println zipped)