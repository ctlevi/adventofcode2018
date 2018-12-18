; (def raw-input (slurp "inputs/day18-test.txt"))
(def raw-input (slurp "inputs/day18.txt"))
(def initial-state
  (->> raw-input
       (clojure.string/split-lines)
       (mapv vec)))

(defn safe-get [state [x y]]
  (if (or
       (< y 0)
       (< x 0)
       (>= y (count state))
       (>= x (count (state y))))
    nil
    (get-in state [y x])))

(defn next-acre-state [state acre [x y]]
  (let [up-left (safe-get state [(dec x) (dec y)])
        up (safe-get state [x (dec y)])
        up-right (safe-get state [(inc x) (dec y)])
        right (safe-get state [(inc x) y])
        right-down (safe-get state [(inc x) (inc y)])
        down (safe-get state [x (inc y)])
        down-left (safe-get state [(dec x) (inc y)])
        left (safe-get state [(dec x) y])
        adjacent-acres (list up-left up up-right right right-down down down-left left)]
    (case acre
      ; open
      \. (if (>= (count (filter #(= % \|) adjacent-acres)) 3)
           \|
           \.)
      ; trees
      \| (if (>= (count (filter #(= % \#) adjacent-acres)) 3)
           \#
           \|)
      ; lumberyard
      \# (if (and
              (some #(= % \#) adjacent-acres)
              (some #(= % \|) adjacent-acres))
           \#
           \.))))

(defn advance-one-minute [state]
  (into []
        (map-indexed
         (fn [y row]
           (into []
                 (map-indexed
                  (fn [x acre] (next-acre-state state acre [x y]))
                  row)))
         state)))

(defn total-resource-value [state]
  (let [state-flat (flatten state)
        wooded-acres (count (filter #(= % \|) state-flat))
        lumberyards (count (filter #(= % \#) state-flat))]
    (* wooded-acres lumberyards)))

(println (->> initial-state
              (iterate advance-one-minute)
              ; Iterate returns the initial-state so we have to drop one extra than you'd think
              (drop 10)
              (first)
              (total-resource-value)))

; PART 2

(def repeated-values [[484 192556]
                      [485 191151]
                      [486 193980]
                      [487 196185]
                      [488 199341]
                      [489 198968]
                      [490 198645]
                      [491 199752]
                      [492 199374]
                      [493 199532]
                      [494 197071]
                      [495 200434]
                      [496 190164]
                      [497 192676]
                      [498 194955]
                      [499 200625]
                      [500 206064]
                      [501 212850]
                      [502 219387]
                      [503 221958]
                      [504 223468]
                      [505 224436]
                      [506 223920]
                      [507 218631]
                      [508 207776]
                      [509 207060]
                      [510 201872]
                      [511 193050]])
; Using below, I found that the above 28 numbers start repeating at minute 484
; (map println (->> initial-state
;                   (iterate advance-one-minute)
;               ; Iterate returns the initial-state so we have to drop one extra than you'd think
;                   (take 1000)
;                   (map-indexed (fn [i s] [i (total-resource-value s)]))))
(println (repeated-values (rem (- 1000000000 484) 28)))