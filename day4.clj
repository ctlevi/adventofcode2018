; (def input (slurp "day4-test.txt"))
(def input (slurp "day4.txt"))

; Parses the input lines into a more friendly list of maps. Each line is parsed into:
;
; [1518-11-01 00:00] Guard #10 begins shift  { :type :shift, :timestamp Date, :minute 0, id: 10 }
; [1518-11-01 00:05] falls asleep            { :type :asleep, :timestamp Date, :minute 5 }
; [1518-11-01 00:25] wakes up                { :type :awake, :timestamp Date, :minute 25 }
(defn convertStringToDate [dateString] (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd hh:mm") dateString))
(defn processInputLine [line]
  (let [[_ timestamp minute type id] (re-find #"\[(\d+-\d+-\d+ \d\d:(\d\d))\] (Guard|falls|wakes) #?(\d+)?" line)
        parsedTime (convertStringToDate timestamp)
        parsedMinute (Integer/parseInt minute)]
    (case type
      "Guard" {:type :shift, :timestamp parsedTime, :minute parsedMinute, :id (Integer/parseInt id)}
      "falls" {:type :asleep, :timestamp parsedTime, :minute parsedMinute}
      "wakes" {:type :awake, :timestamp parsedTime, :minute parsedMinute})))
(def guardLogs
  (->>
   (clojure.string/split-lines input)
   (map processInputLine)
   (sort-by :timestamp)))

(defn between [i range]
  (let [[start end] range]
    (and (<= start i) (>= end i))))

; Will take a guard data structure and increment the asleep times through the asleepRange where
; asleepRange is a 2 element vector, e.g. [2, 5], with the inclusive times they were asleep
(defn incrementAsleepTimes [asleepTimes asleepRange]
  (map-indexed (fn [i asleepCount] (if (between i asleepRange) (inc asleepCount) asleepCount)) asleepTimes))

(defn processLog [state log]
  (let [{currentId :currentId
         time :time
         currentStatus :currentStatus
         asleepTimesByGuardId :asleepTimesByGuardId} state
        {type :type
         minute :minute
         id :id} log
        asleepTimes (get asleepTimesByGuardId currentId (repeat 60 0))]
    (cond
      ; Was awake, but now asleep
      (and (= currentStatus :awake) (= type :asleep))
      (-> state
          (assoc :time minute)
          (assoc :currentStatus type))
      ; Was asleep, but now awake
      (and (= currentStatus :asleep) (= type :awake))
      (-> state
          (assoc :time minute)
          (assoc :currentStatus type)
          (assoc-in [:asleepTimesByGuardId currentId] (incrementAsleepTimes asleepTimes [time (dec minute)])))
      ; Guard change
      (= type :shift)
      (-> state
          (assoc :currentId id)
          (assoc :time 0)
          (assoc :currentStatus :awake))
      ; Should never get here, the input seems to guarantee only the 3 states I enumerated above,
      ; even though there's way more possibly states.
      :default (println "THIS SHOULD NEVER HAPPEN"))))

(defn countAsleepTimes [logs]
  (:asleepTimesByGuardId
   (let [[firstLog & remainingLogs] logs
         state {:currentId (:id firstLog), :time 0, :currentStatus :awake, :asleepTimesByGuardId {}}]
     (reduce processLog state remainingLogs))))

(def strategy1
  (->> guardLogs
       (countAsleepTimes)
       (map (fn [[id counts]] {:id id, :counts counts, :sum (reduce + counts)}))
       (reduce (fn [first second] (if (> (:sum first) (:sum second)) first second)))))

(def answer1
  (let [counts (:counts strategy1)
        m (apply max counts)
        minute (.indexOf counts m)
        id (:id strategy1)]
    (* id minute)))

(def strategy2
  (->> guardLogs
       (countAsleepTimes)
       (map (fn [[id counts]] {:id id, :counts counts, :max (apply max counts)}))
       (reduce (fn [first second] (if (> (:max first) (:max second)) first second)))))

(def answer2
  (let [m (:max strategy2)
        minute (.indexOf (:counts strategy2) m)
        id (:id strategy2)]
    (* id minute)))

(println answer1)
(println answer2)