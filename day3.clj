; (def input ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
(def input
  (->>
   (slurp "day3.txt")
   (clojure.string/split-lines)))

(defn parseInputIntoClaims [rawClaims]
  (mapv
   (fn [rawClaim]
     (let [[_ id fromLeft fromTop width height] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" rawClaim)]
       {:id id,
        :fromLeft (Integer/parseInt fromLeft)
        :fromTop (Integer/parseInt fromTop)
        :width (Integer/parseInt width)
        :height (Integer/parseInt height)}))
   rawClaims))

(def parsedClaims (parseInputIntoClaims input))

; Generate a 1000x1000 2D vector with :.. We will change this to :X and then eventually
; :! to count repeated cuts. Why symbols? Because it prints out nicely in the console
(def uncutFabric
  (into
   []
   (repeat 1000 (into [] (repeat 1000 :.)))))

(defn getCuts [claim]
  (let [xRange (range (:fromLeft claim) (+ (:fromLeft claim) (:width claim)))
        yRange (range (:fromTop claim) (+ (:fromTop claim) (:height claim)))]
    (mapcat (fn [x] (mapv (fn [y] [x y]) yRange)) xRange)))

(defn cutPieceOfFabric [currentCutStatus]
  (case currentCutStatus
    :. :X
    :X :!
    :!))

(defn cutFabric [fabric claim]
  (let [cuts (getCuts claim)]
    (reduce (fn [newFabric cut] (update-in newFabric cut cutPieceOfFabric)) fabric cuts)))

(defn cutAllClaimsInFabric [fabric claims]
  (reduce cutFabric fabric claims))

(def answer1
  (->>
   (cutAllClaimsInFabric uncutFabric parsedClaims)
   (flatten)
   (filter #(= % :!))
   (count)))

(defn doesClaimHaveNoOverlap [fabric claim]
  (let [cuts (getCuts claim)]
    (every? #(= (get-in fabric %) :X) cuts)))

(def answer2
  (->>
   (filter
    (partial doesClaimHaveNoOverlap (cutAllClaimsInFabric uncutFabric parsedClaims))
    parsedClaims)
   (first)
   (:id)))

(println answer1)
(println answer2)