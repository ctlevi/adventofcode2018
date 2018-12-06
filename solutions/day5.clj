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
      ; Looped with nothing
      (nil? a)
      (reverse reacting-polymer)
      ; Looped with only one element
      (and (some? a) (nil? b))
      (reverse (conj reacting-polymer a))
      ; Our actual reacting code
      :else
      (if (destroy? a b)
        (if (empty? reacting-polymer)
          (recur xs reacting-polymer)
          (recur (conj xs (first reacting-polymer)) (rest reacting-polymer)))
        (recur (conj xs b) (conj reacting-polymer a))))))

(def answer1 (count (run-reactions (seq input))))

(println answer1)
; (destroy? \a \b)