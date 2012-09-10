(ns potter
  (:use [clojure.set :only [union intersection]]
        [clojure.math.combinatorics :only [combinations subsets]]))

(defn partition? [P S]
  (and (not (contains? P #{}))
       (= (apply union P) S)
       (every? #(= (intersection (first %) (second %)) #{}) (combinations P 2))))

(defn find-power-set [S]
  (set (map set (subsets S))))

(defn find-all-partitions [S]
  (filter #(partition? % S) (subsets (find-power-set S))))

(defn find-all-basket-partitions-via-power-sets [shopping-basket-books]
  (let [S (set (range (count shopping-basket-books)))]
    (for [P (find-all-partitions S)]
      (for [subset P]
        (map shopping-basket-books subset)))))

(defn find-discounted-subsets [S]
  (remove #(< (count %) 2) (subsets S)))

(defn find-discounted-subsets-alternate [S]
  (mapcat #(combinations S %) (range 2 6)))

(defstruct node :book-freqs-in-basket :selected-book-groups)

(defn remove-from-basket [book-freqs subset]
  (into {} (remove #(zero? (val %)) (reduce #(update-in %1 [%2] dec) book-freqs subset))))

(defn expand-book-freqs [book-freqs]
  (mapcat (fn [[book-id frequency]] (repeat frequency book-id)) book-freqs))

(defn successors [{:keys [book-freqs-in-basket selected-book-groups]}]
  (let [distinct-books (keys book-freqs-in-basket)]
    (if-let [discounted-book-groups (seq (find-discounted-subsets distinct-books))]
      (for [books discounted-book-groups]
        (struct-map node
          :book-freqs-in-basket (remove-from-basket book-freqs-in-basket books)
          :selected-book-groups (cons books selected-book-groups)))
      (let [undiscounted-book-group (expand-book-freqs book-freqs-in-basket)]
        (list (struct-map node
                :book-freqs-in-basket nil
                :selected-book-groups (if (seq undiscounted-book-group)
                                        (cons undiscounted-book-group selected-book-groups)
                                        selected-book-groups)))))))

(defn leaf-node? [node]
  (nil? (:book-freqs-in-basket node)))

(defn find-next-partition [[open-list partition]]
  (if-let [node (first open-list)]
    (if (leaf-node? node)
      [(rest open-list) (:selected-book-groups node)]
      (recur [(concat (successors node) (rest open-list)) nil]))))

(defn find-all-basket-partitions-via-tree-traversal [shopping-basket-books]
  (let [root-node (struct-map node
                    :book-freqs-in-basket (frequencies shopping-basket-books)
                    :selected-book-groups ())]
    (->> [(list root-node) nil]
         (iterate find-next-partition)
         rest
         (take-while seq)
         (map second))))

(defn get-bin-discount [bin]
  (case (count (distinct bin))
    2 0.05
    3 0.10
    4 0.20
    5 0.25
    0.0))

(def base-book-price 8.00)

(defn calculate-bin-cost [bin]
  (* base-book-price (count bin) (- 1.0 (get-bin-discount bin))))

(defn calculate-partition-cost [partition]
  (reduce + (map calculate-bin-cost partition)))

(def find-all-basket-partitions find-all-basket-partitions-via-tree-traversal)

(defn find-minimum-cost-partition-naive [shopping-basket-books]
  (let [all-partitions (find-all-basket-partitions shopping-basket-books)
        all-costs      (map calculate-partition-cost all-partitions)]
    (apply min-key val (zipmap all-partitions all-costs))))

(defn find-minimum-cost-partition-aux [book-freqs-in-basket]
  (if (seq book-freqs-in-basket)
    (let [distinct-books (keys book-freqs-in-basket)]
      (if-let [discounted-book-groups (seq (find-discounted-subsets distinct-books))]
        (apply min-key calculate-partition-cost
               (for [books discounted-book-groups]
                 (cons books (find-minimum-cost-partition-aux (remove-from-basket book-freqs-in-basket books)))))
        (let [undiscounted-book-group (expand-book-freqs book-freqs-in-basket)]
          (list undiscounted-book-group))))))
(def find-minimum-cost-partition-aux (memoize find-minimum-cost-partition-aux))

(defn find-minimum-cost-partition-via-dynamic-programming [shopping-basket-books]
  (let [minimum-cost-partition (find-minimum-cost-partition-aux (frequencies shopping-basket-books))]
    [minimum-cost-partition (calculate-partition-cost minimum-cost-partition)]))
