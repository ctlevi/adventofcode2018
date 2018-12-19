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
    (println instruction-to-exec)
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

; (println (run-program-to-halt input-program))

; Part 2

; EDIT: None of the stuff I did below worked, I ended up having to run the program on paper
;       and convert it to pseudo code to see what it was doing. I then was able to optimize
;       a stupid giant 10-billion operation loop into a single operation and then I just ran
;       the program optimized with some python code.
;
; Going through the program yielded me the code below that I converted to python. You can
; ignore register 4 and register 1 as register 1 is the instruction pointer and register 4
; was being used for scratch space.
; 
; # About 20 startup instructions that ultimately set the registers to the following value.
; # I just found these values using my clojure code, and only analyzed the actual loop code.
; reg0 = 0
; reg2 = 1
; reg3 = 1
; reg5 = 10551296
; 
; while True:
;       while True:
;               if (reg3*reg2) == reg5:
;                       reg0 = reg0 + reg2
;               reg3 = reg3 + 1
;               if reg3 > reg5:
;                       break
;       reg2 = reg2 + 1
;       if reg2 > reg5:
;               # Program will terminate
;               print reg0
;               break
;               reg3 = 1
;
; The inner while loop is taking 10-billion operations just to possibly set reg0 and then try it
; all again when we increment reg2. You can convert the inner while loop to a modulo test like so
;
; if reg5 % reg2 == 0:
;       reg0 = reg0 + reg2
;
; So the full code is, and runs quite quickly:
;
; # About 20 startup instructions that ultimately set the registers to the following value.
; # I just found these values using my clojure code, and only analyzed the actual loop code.
; reg0 = 0
; reg2 = 1
; reg3 = 1
; reg5 = 10551296
; 
; while True:
;       if reg5 % reg2 == 0:
;               reg0 = reg0 + reg2
;       reg2 = reg2 + 1
;       if reg2 > reg5:
;               # Program will terminate
;               print reg0
;               break
;               reg3 = 1


; This prints out the registers every 10,000 run of the program. I noticed register 5 stays
; the same large number, 10551296 and the rest of the registers remain the same too, except
; for Register 3 which is increasing by 2500
; (->> (update input-program :registers #(assoc % 0 1))
;      (iterate run-program-for-one-instruction)
;      (take-nth 10000)
;      (map #(println (:registers %))))

; Here we print out the registers every tick of the program. The start of the program runs around
; ~20 instructions ending with [0 11 1 2 0 10551296]
; Then it starts repeating over and over again what looks like this:
; [0 3 1 2 0 10551296]
; [0 4 1 2 2 10551296]
; [0 5 1 2 0 10551296]
; [0 6 1 2 0 10551296]
; [0 8 1 2 0 10551296]
; [0 9 1 3 0 10551296]
; [0 10 1 3 0 10551296]
; [0 11 1 3 0 10551296]
; Once the registers loop back around (on my input the IP register is Register 1 which we can see incrementing in the loop),
; Register 4 increments. You can see it go from 2 to 3. So the program must be stuck in a loop. Let's see what loop it is doing.
; (->> (update input-program :registers #(assoc % 0 1))
;      (iterate run-program-for-one-instruction)
;      (take-nth 1)
;      (map #(println (:registers %))))

; I added a print statement to run-program-for-one-instruction to print out the instructions and ran below
; I found the following instructions to start looping:
; (:eqrr 4 5 4)
; (:addr 4 1 1)
; (:addi 1 1 1)
; (:addi 3 1 3)
; (:gtrr 3 5 4)
; (:addr 1 4 1)
; (:seti 2 4 1)
; (:mulr 2 3 4)
; The program looks like it will end once Register 3 is greater than register 5. I'm basing this off of
; (:gtrr 3 5 4) instruction above. I think we can just run the program with the registers at a large 
; jump forward and see when it crashes.
; (->> (update input-program :registers #(assoc % 0 1))
;      (iterate run-program-for-one-instruction)
;      (take 40)
;      (map #(println (:registers %))))

; Running the program with registers set close to where I think it will jump out of the loop, based on
; the register output when I was dumping state every 10,000 time. Where did I get 1248? It's just the first
; dump of the program after 10,000 runs. 1250 is how much the program state changes after another 10,000 runs
; So we will be advancing 10,000 * 8,300 program states with the code below
; thing that got printed before I ctrl-c-ed
; (->> (assoc input-program :registers [1 6 27 (+ 39 (* 1250 8400)) 0 10551296])
;      (iterate run-program-for-one-instruction)
;      (take-nth 10000)
;      (map #(println (:registers %))))
;      (drop-while #(false? (:crashed %)))
;      (first))