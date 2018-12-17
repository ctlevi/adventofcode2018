(def raw-input (slurp "inputs/day16.txt"))

(def input-samples (->> raw-input
                        (clojure.string/split-lines)
                        (take 3128)
                        (partition 4)
                        (map (fn [[before instruction after]]
                               (let [parse-registers (fn [input]
                                                       (let [[_ registers-string] (re-find #"\[(.*)\]" input)]
                                                         (mapv #(Integer/parseInt %) (clojure.string/split registers-string #", "))))]
                                 {:before (parse-registers before)
                                  :after (parse-registers after)
                                  :instruction (mapv #(Integer/parseInt %) (clojure.string/split instruction #" "))})))))

(def opcodes
  {:addr (fn [registers [_ a b c]]
           (assoc registers c (+ (registers a) (registers b))))

   :addi (fn [registers [_ a b c]]
           (assoc registers c (+ (registers a) b)))

   :mulr (fn [registers [_ a b c]]
           (assoc registers c (* (registers a) (registers b))))

   :muli (fn [registers [_ a b c]]
           (assoc registers c (* (registers a) b)))

   :banr (fn [registers [_ a b c]]
           (assoc registers c (bit-and (registers a) (registers b))))

   :bani (fn [registers [_ a b c]]
           (assoc registers c (bit-and (registers a) b)))

   :borr (fn [registers [_ a b c]]
           (assoc registers c (bit-or (registers a) (registers b))))

   :bori (fn [registers [_ a b c]]
           (assoc registers c (bit-or (registers a) b)))

   :setr (fn [registers [_ a _ c]]
           (assoc registers c (registers a)))

   :seti (fn [registers [_ a _ c]]
           (assoc registers c a))

   :gtir (fn [registers [_ a b c]]
           (assoc registers c (if (> a (registers b)) 1 0)))

   :gtri (fn [registers [_ a b c]]
           (assoc registers c (if (> (registers a) b) 1 0)))

   :gtrr (fn [registers [_ a b c]]
           (assoc registers c (if (> (registers a) (registers b)) 1 0)))

   :eqir (fn [registers [_ a b c]]
           (assoc registers c (if (= a (registers b)) 1 0)))

   :eqri (fn [registers [_ a b c]]
           (assoc registers c (if (= (registers a) b) 1 0)))

   :eqrr (fn [registers [_ a b c]]
           (assoc registers c (if (= (registers a) (registers b)) 1 0)))})

(defn opcodes-that-match [{before-reg :before, instruction :instruction, after-reg :after}]
  (into #{} (map key (filter (fn [[opcode execution]] (= after-reg (execution before-reg instruction))) opcodes))))

(println (->>
          input-samples
          (map opcodes-that-match)
          (filter #(>= (count %) 3))
          (count)))

(def input-program (->> raw-input
                        (clojure.string/split-lines)
                        (drop 3130)
                        (map
                         (fn [instruction]
                           (mapv
                            #(Integer/parseInt %)
                            (clojure.string/split instruction #" "))))))

(def opcode-numbers
  {0 :eqri
   1 :bori
   2 :addi
   3 :bani
   4 :seti
   5 :eqrr
   6 :addr
   7 :gtri
   8 :borr
   9 :gtir
   10 :setr
   11 :eqir
   12 :mulr
   13 :muli
   14 :gtrr
   15 :banr})

; Used the below to incrementally determine the opcode numbers above by hand. I just kept running
; below and narrowing it down by 1, then adding it to the map above if I could narrow it down. I'm
; sure I could have written a function to do it all, but whatever
(defn find-opcode-number [opcode]
  (clojure.set/difference
   (into #{}
         (map
          (fn [[code]] code)
          (filter
           (fn [[_ possibles]] (contains? possibles opcode))
           (map (fn [sample] [((:instruction sample) 0) (opcodes-that-match sample)]) input-samples))))
   (into #{} (keys opcode-numbers))))
(doseq [[opcode] opcodes]
  (println opcode " " (find-opcode-number opcode)))

(println
 (reduce
  (fn [registers [opcode-num :as instruction]] (((get opcode-numbers opcode-num) opcodes) registers instruction))
  [0 0 0 0]
  input-program))