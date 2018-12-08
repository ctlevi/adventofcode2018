; (def raw-input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(def raw-input (slurp "inputs/day8.txt"))

(defn parse-raw-input [input]
  (map #(Integer/parseInt %) (clojure.string/split input #" ")))

(defn build-tree-helper [input]
  (let [[child-node-count metadata-count & remaining] input
        [children remaining-after-children]  (reduce
                                              (fn [[child-list remaining] _]
                                                (let [{remaining-after :remaining, node :node} (build-tree-helper remaining)]
                                                  [(conj child-list node) remaining-after]))
                                              ['[] remaining]
                                              (repeat child-node-count nil))
        metadata (take metadata-count remaining-after-children)
        remaining-after-all (drop metadata-count remaining-after-children)]
    {:remaining remaining-after-all,
     ; Here is what the tree data structure actually looks like
     :node      {:header [child-node-count metadata-count],
                 :children children,
                 :metadata metadata}}))
(defn build-tree [input]
  (->> input
       (parse-raw-input)
       (build-tree-helper)
       (:node)))

(defn count-metadata [node]
  (+
   (reduce + (:metadata node))
   (reduce + (map count-metadata (:children node)))))

(def answer1
  (->> raw-input
       (build-tree)
       (count-metadata)))

(println answer1)

(defn build-child-metadata-list [children metadata]
  (map
   (fn [entry]
     (let [zero-node {:children [] :metadata '()}]
       (if (= 0 entry)
         zero-node
         (get children (- entry 1) zero-node))))
   metadata))

(defn count-part-2 [node]
  (let [{children :children, metadata :metadata} node]
    (if (empty? children)
      (reduce + metadata)
      (reduce + (map count-part-2 (build-child-metadata-list children metadata))))))

(def answer2
  (->> raw-input
       (build-tree)
       (count-part-2)))

(println answer2)
