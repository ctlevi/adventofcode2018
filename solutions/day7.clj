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

(defn get-step-time [step]
  (-> step
      (.charAt 0)
      (int)
      (- 64)
      (+ 60)))

(defn find-next-steps [dependencies]
  (let [remaining-steps (into #{} (keys dependencies))
        steps-not-yet-ready (into #{} (flatten (vals dependencies)))]
    (apply sorted-set (clojure.set/difference remaining-steps steps-not-yet-ready))))

(def max-workers 5)
; Worker looks like { :time-left 34,  :step "A" }, workers is a list of these
(defn advance-time [dependencies workers]
  (let [workers-after-next-second (map #(update % :time-left dec) workers)
        steps-just-finished (map :step (filter (fn [worker] (= 0 (:time-left worker))) workers-after-next-second))
        workers-still-working (filter (fn [worker] (> (:time-left worker) 0)) workers-after-next-second)
        new-workers-to-add (- max-workers (count workers-still-working))
        new-dependencies (reduce dissoc dependencies steps-just-finished)
        steps-still-working (into #{} (map :step workers-still-working))
        next-steps (clojure.set/difference (find-next-steps new-dependencies) steps-still-working)]
    [new-dependencies, (->> next-steps
                            (take new-workers-to-add)
                            (map (fn [step] {:time-left (get-step-time step), :step step}))
                            (concat workers-still-working))]))

(def answer2
  (loop [dependencies dependencies-map
         workers '()
         time 0]
    (if (and (not= time 0) (empty? workers))
      (- time 1)
      (let [[next-dependencies next-workers] (advance-time dependencies workers)]
        (recur next-dependencies next-workers (inc time))))))

(println answer1)
(println answer2)