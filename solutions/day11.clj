; (def my-grid-serial-number 18)
(def my-grid-serial-number 6392)

(defn get-power-level [[x y] grid-serial-number]
  (let [rack-id (+ x 10)]
    (-> rack-id
        (* y)
        (+ grid-serial-number)
        (* rack-id)
        ; Get the 100s digit
        (quot 100)
        (mod 10)
        ; Finally the power-level
        (- 5))))

(defn to-1-based-point [x y]
  [(inc x) (inc y)])

(defn make-power-grid [grid-size grid-serial-number]
  (let [grid-indices (range 0 grid-size)]
    (mapv
     (fn [y] (mapv (fn [x] (get-power-level (to-1-based-point x y) grid-serial-number)) grid-indices))
     grid-indices)))

(defn sum-power [[x y] grid]
  (let [points-to-sum (mapcat
                       (fn [y] (map (fn [x] [x y]) (range x (+ x 3))))
                       (range y (+ y 3)))]
    (reduce (fn [total [x y]] (+ total ((grid x) y))) 0 points-to-sum)))

(defn largest-total-power [grid]
  (let [points-range (range 0 (- (count (grid 0)) 2))
        points-to-check (mapcat
                         (fn [y] (map (fn [x] [x y]) points-range))
                         points-range)]
    (reduce
     (fn [state point]
       (let [curr-max (:max state)
             power-at-point (sum-power point grid)]
         (if (> power-at-point curr-max)
           {:max power-at-point, :point (to-1-based-point (point 1) (point 0))}
           state)))
     {:max -10000, :point []}
     points-to-check)))

; (dorun (map println (make-power-grid 10 my-grid-serial-number)))
(println (largest-total-power (make-power-grid 300 my-grid-serial-number)))

(def sum-at-point
  (memoize (fn [[x y] size grid]
             (cond
               ; The base case, just return the value at the grid index
               (= size 1) ((grid y) x)
               ; Break into 4 squares and add up sum-at-point
               (even? size) (let [middle (/ size 2)
                                  top-left [x y]
                                  top-right [(+ x middle) y]
                                  bottom-left [x (+ y middle)]
                                  bottom-right [(+ x middle) (+ y middle)]]
                              (+
                               (sum-at-point top-left middle grid)
                               (sum-at-point top-right middle grid)
                               (sum-at-point bottom-left middle grid)
                               (sum-at-point bottom-right middle grid)))
               ; Else, add up the odd row and add sum-at-point for the even size square immediately
               ; smaller than the current square, i.e. size == 5 would add up the top row and left column
               ; and then call sum-at-point on the 4x4 square starting diagonally to it
               :else (let [top-row-sum (reduce + (map #((grid y) %) (range x (+ x size))))
                           ; This makes sure to not include the point at x,y which is already included in top-row-sum
                           left-column-sum (reduce + (map #((grid %) x) (range (inc y) (+ y size))))]
                       (+
                        top-row-sum
                        left-column-sum
                        (sum-at-point [(inc x) (inc y)] (dec size) grid)))))))

; UHHHHHHHHHH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! I eventually got it to work using the code below.
; This helped me figure out that the memoize was not memoizing everything. It seemed like Clojure
; was running the java process with 2GB and was being intelligent about not using it all up with
; memoize. So I guess it was evicting some of the cache......Which made the algorithm insanely slow
; So instead I ran this like so: clojure -J-Xmx4g solutions/day11.clj and it worked pretty quickly!
; (defn largest-total-power-any-size [grid]
;   (let [grid-size (count (grid 0))
;         points-range (range 0 grid-size)
;         points-to-check (mapcat
;                          (fn [y] (map (fn [x] [x y]) points-range))
;                          points-range)
;         points (atom (doall (mapcat
;                              (fn [point]
;                                (let [[x y] point
;                                      to-end-x (- grid-size x)
;                                      to-end-y (- grid-size y)
;                                      size-range (range 1 (inc (min to-end-x to-end-y)))]
;                                  (map (fn [size] [point size]) size-range)))
;                              points-to-check)))
;         state (atom {:max -10000, :point [], :size 0})]
;     (do
;       (doseq [[point size] @points]
;         (let [[point size] (first @points)
;               curr-max (:max @state)
;               power-at-point (sum-at-point point size grid)]
;           (if (> power-at-point curr-max)
;             (do
;               (compare-and-set! state @state {:max power-at-point, :point (to-1-based-point (point 0) (point 1)), :size size})
;               (println @state))))
;         (swap! points rest))
;       @state)))

; This was my original solution that ran way too slow, but only because I didn't know about needing to give the JVM more memory
; which I learned with the above implementation. With 4GB of space, it runs in about 5 minutes. Better than nothing!
(defn largest-total-power-any-size [grid]
  (let [grid-size (count (grid 0))
        points-range (range 0 grid-size)
        points-to-check (mapcat
                         (fn [y] (map (fn [x] [x y]) points-range))
                         points-range)
        points-to-check-with-size (mapcat
                                   (fn [point]
                                     (let [[x y] point
                                           to-end-x (- grid-size x)
                                           to-end-y (- grid-size y)
                                           size-range (range 1 (inc (min to-end-x to-end-y)))]
                                       (map (fn [size] [point size]) size-range)))
                                   points-to-check)]
    (reduce
     (fn [state [point size]]
       (let [curr-max (:max state)
             power-at-point (sum-at-point point size grid)]
        ; Nice progress indicator here if you uncomment
        ;  (if (and (zero? (point 0)) (= size 1))
        ;    (println point))
         (if (> power-at-point curr-max)
           {:max power-at-point, :point (to-1-based-point (point 0) (point 1)), :size size}
           state)))
     {:max -10000, :point [], :size 0}
     points-to-check-with-size)))

(println (largest-total-power-any-size (make-power-grid 300 my-grid-serial-number)))