; (def input (slurp "inputs/day6-test.txt"))
(def input (slurp "inputs/day6.txt"))
(def input-coordinates
  (->> input
       (clojure.string/split-lines)
       (map
        (fn [string-coord]
          (mapv #(Integer/parseInt %) (clojure.string/split string-coord #", "))))
       (into [])))

(def grid-size 400)
(def base-grid
  (repeat grid-size (repeat grid-size nil)))

(defn distance [x y sx sy]
  (+ (Math/abs (- x sx)) (Math/abs (- y sy))))

(defn find-min-index [coll]
  (let [min (apply min coll)
        min-count (count (filter #(= min %) coll))
        min-index (.indexOf coll min)]
    ; (cond
    ;   (> min-count 1) -1
    ;   (= min 0) -2
    ;   (= min-count 1) min-index)))
    (if (> min-count 1) -1 min-index)))

(defn convert-to-char [n]
  (case n
    -2 "*"
    -1 "."
    0 "A"
    1 "B"
    2 "C"
    3 "D"
    4 "E"
    5 "F"
    :default (throw (Exception. "Remove convert-to-char"))))

(defn closest-coordinate [x y coordinates]
  (->> coordinates
       (map (fn [[coord-x coord-y]] (distance x y coord-x coord-y)))
       (find-min-index)))

(defn generate-grid-with-min-distance [grid coordinates]
  (map-indexed
   (fn [y row] (map-indexed
                (fn [x _] (closest-coordinate x y coordinates))
                row))
   grid))

(defn find-infinite-coords [grid]
  (let [top-row (first grid)
        right-column (map last grid)
        bottom-row (last grid)
        left-column (map first grid)]
    (into #{} (distinct (concat top-row right-column bottom-row left-column)))))

(def answer1
  (let [grid (generate-grid-with-min-distance base-grid input-coordinates)
        infinite-coords (find-infinite-coords grid)]
    (->> grid
         (flatten)
         (frequencies)
         (filter (fn [[k v]] (not (contains? infinite-coords k))))
         (map (fn [[_ area]] area))
         (apply max))))
(println answer1)