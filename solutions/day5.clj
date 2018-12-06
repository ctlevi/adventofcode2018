; (def input "dabAcCaCBAcCcaDA")
(def input (slurp "inputs/day5.txt"))

(defn destroy? [first second]
  (and
   (not= first second)
   (= (Character/toUpperCase first) (Character/toUpperCase second))))

(defn run-reactions [polymer]
  (loop [[a b & xs] polymer
         reacting-polymer '()]
    (cond
      ; Looped with nothing, we reverse at the end because we were using seq conj
      (nil? a)
      (reverse reacting-polymer)
      ; Looped with only one element, we reverse at the end because we were using seq conj
      (and (some? a) (nil? b))
      (reverse (conj reacting-polymer a))
      ; Our actual reacting code
      :else
      (if (destroy? a b)
        ; If we destroy, the letter immediately before the two destroyed letters might react, take for example AbBa.
        ; So we remove it from the list we are building up, reacting-polymer, and add it back to the polymer, and
        ; see if it reacts with the next letter. 
        (if (empty? reacting-polymer)
          (recur xs reacting-polymer)
          (recur (conj xs (first reacting-polymer)) (rest reacting-polymer)))
        ; No destroy, we we continue building up our lists and looking at the next letters
        (recur (conj xs b) (conj reacting-polymer a))))))

(def answer1 (count (run-reactions (seq input))))

(defn remove-type [type polymer]
  (filter #(not= (Character/toLowerCase %) type) polymer))

(def answer2
  (->> (seq "abcdefghijklmnopqrstuvwxyz")
       (map #(count (run-reactions (remove-type % (seq input)))))
       (apply min)))

(println answer1)
(println answer2)