; (def raw-input (slurp "inputs/day19-test.txt"))
(def raw-input (slurp "inputs/day19.txt"))

(def input-program
  (let [[ip-string & instruction-strings] (clojure.string/split-lines raw-input)
        [_ ip] (clojure.string/split ip-string #" ")
        instructions (mapv
                      (fn [instruction]
                        (let [[opcode & arguments] (clojure.string/split instruction #" ")]
                          (concat
                           (list (keyword opcode))
                           (map #(Integer/parseInt %) arguments))))
                      instruction-strings)]

    {:ip-register (Integer/parseInt ip),
     :instructions instructions
     :crashed false
     :registers [0 0 0 0 0 0]}))

; Copied from day16.clj
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

(defn run-instruction [registers instruction]
  ((get opcodes (first instruction)) registers instruction))

(defn run-program-for-one-instruction [program]
  (let [{ip-register :ip-register,
         instructions :instructions,
         registers :registers} program
        ip (get registers ip-register)
        instruction-to-exec (get instructions ip)
        new-registers (run-instruction registers instruction-to-exec)
        next-ip (inc (get new-registers ip-register))]
    (if (or
         (>= next-ip (count instructions))
         (< next-ip 0))
      (-> program
          (assoc :registers new-registers)
          (assoc :crashed true))
      (-> program
          (assoc :registers (assoc new-registers ip-register next-ip))))))

(defn run-program-to-halt [program]
  (->> program
       (iterate run-program-for-one-instruction)
       (drop-while #(false? (:crashed %)))
       (first)))

(println (run-program-to-halt input-program))