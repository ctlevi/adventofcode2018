; (def raw-input (slurp "inputs/day12-test.txt"))
(def raw-input (slurp "inputs/day12.txt"))

(defn parse-state-and-rules [input]
  (let [lines (clojure.string/split-lines input)
        [_ start-pots] (re-find #"initial state: (.*)" (first lines))
        rules (map
               (fn [rule-string]
                 (let [[_ match outcome] (re-find #"(.*) => (.)" rule-string)]
                   {:match match, :outcome outcome}))
               (drop 2 lines))]
    {:pots start-pots, :rules rules, :first 0}))

(defn find-matching-rule [pots rules index]
  (let [start (max 0 (- index 2))
        end (min (count pots) (+ index 3))
        section (subs pots start end)
        extended-section (cond
                           (zero? index) (str ".." section)
                           (= 1 index) (str "." section)
                           (= 1 (- (count pots) index)) (str section "..")
                           (= 2 (- (count pots) index)) (str section ".")
                           :else section)]
    (first
     (filter
      #(= (:match %) extended-section)
      rules))))

(defn advance-one-generation [state]
  (let [{pots :pots,
         rules :rules,
         first :first} state
        extend-front (clojure.string/includes? (subs pots 0 2) "#")
        extend-back (clojure.string/includes? (subs pots (- (count pots) 2)) "#")
        pots-to-use (cond
                      (and extend-front extend-back) (str ".." pots "..")
                      extend-front (str ".." pots)
                      extend-back (str pots "..")
                      :else pots)]
    {:first (if extend-front (- first 2) first),
     :rules rules
     :pots (clojure.string/join
            (map-indexed
             (fn [i _]
               (let [matching-rule (find-matching-rule pots-to-use rules i)]
                 (if (nil? matching-rule)
                   "."
                   (:outcome matching-rule))))
             pots-to-use))}))

(defn score-state [state]
  (let [{pots :pots, first :first} state]
    (reduce
     +
     (map-indexed
      (fn [i pot] (if (= pot \#) (+ i first) 0))
      pots))))

(score-state (reduce
              (fn [state _] (advance-one-generation state))
              (parse-state-and-rules raw-input)
              (repeat 20 nil)))