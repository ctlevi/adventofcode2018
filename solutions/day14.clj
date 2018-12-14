; (def recipe-count 2018)
(def recipe-count 77201)

(defn make-new-recipes [{scores :scores, current-recipes-indexes :current-recipes-indexes :as state}]
  (let [current-recipes (map scores current-recipes-indexes)
        sum-score (reduce + current-recipes)
        possible-next-score (quot sum-score 10)
        next-score (rem sum-score 10)
        new-scores (conj
                    (if (zero? possible-next-score) scores (conj scores possible-next-score))
                    next-score)
        next-recipes-indexes (mapv (fn [i] (mod (+ 1 (new-scores i) i) (count new-scores))) current-recipes-indexes)]
    (-> state
        (assoc :scores new-scores)
        (assoc :current-recipes-indexes next-recipes-indexes))))

(println (->> {:scores [3 7] :current-recipes-indexes [0 1]}
              (iterate make-new-recipes)
              (map :scores)
              (take-while #(<= (count %) (+ recipe-count 20)))
              (last)
              (drop recipe-count)
              (take 10)
              (clojure.string/join)))

(def recipe-chain [0 7 7 2 0 1])
; (def recipe-chain [5 9 4 1 4])

(defn chain-starts-at [{chain :chain, scores :scores, last-index :last-index}]
  (loop [index last-index]
    (if (>= index (- (count scores) (count chain)))
      [index false]
      (if (= (subvec scores index (+ index (count chain))) chain)
        [index true]
        (recur (inc index))))))

(defn make-new-recipes-and-check-chain [state]
  (let [next-state (make-new-recipes state)
        [last-index found] (chain-starts-at next-state)]
    (-> next-state
        (assoc :last-index last-index)
        (assoc :found found))))

(println (->> {:scores [3 7], :current-recipes-indexes [0 1], :chain recipe-chain, :found false, :last-index 0}
              (iterate make-new-recipes-and-check-chain)
              (take-while #(false? (:found %)))
              (last)
              ; take-while will not include the very last iteration that was not nil,
              ; so make one more set of recipes
              (make-new-recipes)
              (chain-starts-at)))
