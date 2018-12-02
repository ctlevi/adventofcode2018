; (def input ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
; (def input ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
(def input
  (->>
   (slurp "day2.txt")
   (clojure.string/split-lines)))

(def answer1
  (->>
   ; Split into letters
   (mapv vec input)
   ; Get the count of each letter
   (mapv frequencies)
   ; Loop over the counts and create a map of how many contain exactly 2 and/or exactly 3 counts of a letter
   (reduce
    (fn [sums count]
      (let [someExactly3 (some (fn [[k v]] (= v 3)) count)
            someExactly2 (some (fn [[k v]] (= v 2)) count)]
        (cond
          (and someExactly2 someExactly3) (-> (update sums :two inc) (update :three inc))
          (true? someExactly2) (update sums :two inc)
          (true? someExactly3) (update sums :three inc)
          :else sums)))
    {:two 0 :three 0})
   ; Multiple :two and :three together. Probably a much easier way...
   (reduce (fn [total [k v]] (* total v)) 1)))

(defn generatePairsFromIndex [coll index]
  (let [element (get coll index)]
    (map #(vector element %) (nthrest coll (+ index 1)))))
(defn generatePairs [coll]
  (mapcat (partial generatePairsFromIndex coll) (range (count coll))))

(defn differBy [first second]
  (->>
   (mapv vector first second)
   (reduce (fn [sum [x y]] (if (not= x y) (inc sum) sum)) 0)))

(defn sameLetters [first second]
  (->>
   (mapv vector first second)
   (reduce (fn [same [x y]] (if (= x y) (conj same x) same)) [])
   (clojure.string/join "")))

(defn returnPairIfDifferBy [amount [first second :as pair]]
  (if (= (differBy first second) amount) pair nil))

(def answer2
  (->>
   (generatePairs input)
   (map (partial returnPairIfDifferBy 1))
   (filter not-empty)
   (first)
   (apply sameLetters)))

(println answer1)
(println answer2)