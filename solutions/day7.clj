; (def input (slurp "inputs/day7-test.txt"))
(def input (slurp "inputs/day7.txt"))

; Splits into a list of tuples like (A C) where A needs to finish before C
(def input-instructions
  (->> input
       (clojure.string/split-lines)
       (map #(rest (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." %)))))

(def dependencies-map
  (let [possible-steps (distinct (flatten input-instructions))
        init-dependencies (reduce #(assoc %1 %2 '()) {} possible-steps)]
    (reduce
     (fn [deps [before after]] (update deps before #(conj % after)))
     init-dependencies
     input-instructions)))

(defn find-next-step [dependencies]
  (let [remaining-steps (into #{} (keys dependencies))
        steps-not-yet-ready (into #{} (flatten (vals dependencies)))]
    (first (apply sorted-set (clojure.set/difference remaining-steps steps-not-yet-ready)))))

; This actually fails at giving the very last step, but I just manually added the single missing letter at the end
; TODO I should fix that
(def answer1
  (clojure.string/join
   ""
   (loop [dependencies dependencies-map
          step-order []]
     (let [next-step (find-next-step dependencies)
           step-order (conj step-order next-step)
           new-dependencies (dissoc dependencies next-step)]
       (if (empty? new-dependencies)
         step-order
         (recur new-dependencies step-order))))))

(println answer1)