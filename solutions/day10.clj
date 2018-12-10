; (def raw-input (slurp "inputs/day10-test.txt"))
(def raw-input (slurp "inputs/day10.txt"))

(defn parse-point [point-string]
  (let [[_ x y vel-x vel-y] (re-find #"position=<(.*), (.*)> velocity=<(.*), (.*)>" point-string)]
    {:x (Integer/parseInt (clojure.string/trim x)),
     :y (Integer/parseInt (clojure.string/trim y)),
     :vel-x (Integer/parseInt (clojure.string/trim vel-x)),
     :vel-y (Integer/parseInt (clojure.string/trim vel-y))}))

(def starting-points
  (->> raw-input
       (clojure.string/split-lines)
       (map parse-point)))

(defn draw-points [points]
  (let [xs (map :x points)
        ys (map :y points)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)
        points-map (reduce #(assoc %1 [(:x %2) (:y %2)] true) {} points)]
    ; {:min-x min-x :max-x max-x :min-y min-y :max-y max-y, :area (* (- max-x min-x) (- max-y min-y))}))
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (if (nil? (get points-map [x y])) (print " ") (print "#")))
      (print "\n"))))

; Advance the points the number of seconds given by time
(defn advance-points [points time]
  (map
   (fn [{x :x,
         y :y,
         vel-x :vel-x,
         vel-y :vel-y,
         :as point}]
     (-> point
         (assoc :x (+ x (* time vel-x)))
         (assoc :y (+ y (* time vel-y)))))
   points))

; I did part1 and part2 kind of by accident. I just started putting a number of seconds in and I modified draw-points
; to return the area we were drawing. I got lucky and put 10,000 and noticied a pretty small number. Than I just toyed
; with it, changing the number to make the area go down, until I found a few numbers that were making the area go up and
; then down. Eventually I got to 10,027 which was the answer because 10,028 and 10,026 were both making the area bigger
(draw-points (advance-points starting-points 10027))
