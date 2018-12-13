; (def raw-input (slurp "inputs/day13-test.txt"))
(def raw-input (slurp "inputs/day13.txt"))

(defn parse-initial-state [input]
  (let [tracks-with-carts (mapv vec (clojure.string/split-lines input))
        tracks (into []
                     (for [y (range 0 (count tracks-with-carts))]
                       (mapv
                        #(case (get-in tracks-with-carts [y %])
                           (\> \<) \-
                           (\^ \v) \|
                           (get-in tracks-with-carts [y %]))
                        (range 0 (count (tracks-with-carts y))))))
        carts (for [y (range 0 (count tracks-with-carts))
                    x (range 0 (count (tracks-with-carts 1)))
                    :let [cart (get-in tracks-with-carts [y x])]
                    :when (or (= \^ cart) (= \v cart) (= \> cart) (= \< cart))]
                {:position [x y],
                 :next-turn :left,
                 :direction (case cart
                              \> :right
                              \< :left
                              \^ :up
                              \v :down)})]
    {:tracks tracks, :carts carts}))

(defn print-state [state]
  (let [{tracks :tracks, carts :carts} state
        direction-to-char (fn [direction]
                            (case direction
                              :right \>
                              :left \<
                              :up \^
                              :down \v))]
    (doseq [[y row] (map-indexed (fn [i r] [i r]) tracks)]
      (doseq [[x char] (map-indexed (fn [i c] [i c]) row)]
        (let [possible-cart (first (filter #(= (:position %) [x y]) carts))]
          (if (nil? possible-cart)
            (print char)
            (print (direction-to-char (:direction possible-cart))))))
      (println))))

(defn next-turn-from-perspective [direction next-turn]
  (case next-turn
    :straight direction
    :left (case direction
            :left :down
            :right :up
            :up :left
            :down :right)
    :right (case direction
             :left :up
             :right :down
             :up :right
             :down :left)))

(defn turn-after [next-turn]
  (case next-turn
    :left :straight
    :straight :right
    :right :left))

(defn next-cart [tracks
                 {position :position, direction :direction, next-turn :next-turn}]
  (let [[x y] (case direction
                :left (update position 0 dec)
                :right (update position 0 inc)
                :down (update position 1 inc)
                :up (update position 1 dec))
        next-track (get-in tracks [y x])
        next-direction (case next-track
                         \/ (case direction
                              :left :down
                              :right :up
                              :up :right
                              :down :left)
                         \\ (case direction
                              :left :up
                              :right :down
                              :up :left
                              :down :right)
                         \+ (next-turn-from-perspective direction next-turn)
                         (\- \|) direction)]
    {:position [x y],
     :direction next-direction,
     :next-turn (if (= next-track \+) (turn-after next-turn) next-turn)}))

(defn find-crash [carts]
  (let [grouped (group-by #(:position %) carts)
        crash (first (filter (fn [[_ v]] (> (count v) 1)) grouped))]
    (if (nil? crash) nil (key crash))))

(defn move-carts-until-crash [{carts :carts, tracks :tracks}]
  (loop [new-carts '()
         [cart & existing-carts] carts]
    (if (nil? cart)
      new-carts
      (let [new-cart (next-cart tracks cart)
            next-new-carts (conj new-carts new-cart)
            carts-to-test (concat next-new-carts existing-carts)]
        (if (not (nil? (find-crash carts-to-test)))
          carts-to-test
          (recur next-new-carts existing-carts))))))

; Will try to run a full tick, but if a crash happens in the middle of
; running the tick, it will return that state at the point of the crash.
; Crashes can happen in the middle of ticks, not just at the end (which
; I was originally testing for and got a wrong answer)
(defn run-tick-until-crash [state]
  (assoc state :carts
         (let [{tracks :tracks, carts :carts} state]
          ; Sort by y and then x position
           (sort-by
            (fn [{position :position}] [(position 1) (position 0)])
            (move-carts-until-crash state)))))

(println (->> raw-input
              (parse-initial-state)
              (iterate run-tick-until-crash)
              (map #(find-crash (:carts %)))
              (filter #(not= nil %))
              (first)))