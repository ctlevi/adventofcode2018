; (require '[clojure.core.rrb-vector :as fv])
(require '[clojure.data.finger-tree :as ft])

; (def num-players 9)
; (def last-marble 25)
; (def num-players 13)
; (def last-marble 7999)
; No need for an input file, just hand change this
(def num-players 412)
; Part 1
; (def last-marble 71646)
; Part 2
(def last-marble (* 100 71646))

; Hmmmm... Using rrb-vector was throwing ArrayIndexOutOfBoundsException: 33 but using
; regular vectors actually works...
; But then I realized I could never solve part 2 fast enough, so I used the finger-tree
; library... It was a little bit faster but took an INSANELY long time, but it eventually
; split out the answer.
; I originally tried using a circular linked list. But since Clojure is immutable, I could
; not figure out how to make a circular list. So instead, I went with this horrible slow
; approach and just let it run... I might try this in Rust or something else to see how
; fast and awesome linked lists would be for this

; For rrb.vectors
; (defn insert-fast [v i e]
;   (fv/catvec (conj (fv/subvec v 0 i) e) (fv/subvec v i)))
; For regular vectors
; (defn insert-fast [v i e]
;   (into [] (concat (conj (subvec v 0 i) e) (subvec v i))))
(defn insert-fast [v i e]
  (let [[left split-el right] (ft/ft-split-at v i)]
    (if (nil? split-el)
      (ft/ft-concat (conj left e) right)
      (ft/ft-concat (conj (conj left e) split-el) right))))

; For rrb.vectors
; (defn remove-fast [v i]
;   (fv/catvec (fv/subvec v 0 i) (fv/subvec v (inc i))))
; For regular vectors
; (defn remove-fast [v i]
;   (into [] (concat (subvec v 0 i) (subvec v (inc i)))))
(defn remove-fast [v i]
  (let [[left _ right] (ft/ft-split-at v i)]
    (ft/ft-concat left right)))

(defn handle-marble-normal [board last-index players marble]
  (let [board-len (count board)
        next-index (+ last-index 2)
        next-index-wrapped (if (> next-index board-len) 1 next-index)]
    {:board (insert-fast board next-index-wrapped marble),
     :last-index next-index-wrapped,
     :players players}))

(defn get-player-index-from-marble [players marble]
  ; We have to decrement because the zero marble is placed by no player
  (let [index
        (dec (mod marble (count players)))
        index-wrapped (if (neg? index) (dec (count players)) index)]
    index-wrapped))

(defn handle-marble-multiple-of-23 [board last-index players marble]
  (let [board-len (count board)
        index-to-remove (- last-index 7)
        index-to-remove-wrapped (if (neg? index-to-remove) (+ (count board) index-to-remove) index-to-remove)
        next-index-wrapped (if (= index-to-remove-wrapped (dec board-len)) 0 index-to-remove-wrapped)
        removed (get board index-to-remove-wrapped)
        player-index (get-player-index-from-marble players marble)]
    {:board (remove-fast board next-index-wrapped)
     :last-index next-index-wrapped
     :players (update players player-index #(+ % removed marble))}))

(defn play-game-turn [state marble]
  (let [{board :board
         last-index :last-index
         players :players} state
        mult-of-23 (zero? (mod marble 23))]
    (if mult-of-23
      (handle-marble-multiple-of-23 board last-index players marble)
      (handle-marble-normal board last-index players marble))))

(apply
 max
 (:players (reduce
            play-game-turn
            {:board (ft/counted-double-list 0 2 1 3), :last-index 3, :players (into [] (repeat num-players 0))}
            (range 4 (inc last-marble)))))