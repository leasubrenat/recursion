(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll)) true false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (not (empty? (rest coll))) (my-last (rest coll)) (first coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [count-one (count seq-1)
        count-two (count seq-2)]
    (if (> count-one count-two) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (recur pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (= a-seq b-seq)
      true
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      ()
    :else
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? n)
      0
    (zero? k)
      1
    :else
      (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (or (zero? n) (= 1 n))
      n
    :else
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1)
      ()
    :else
      (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= up-to 0)
      ()
    :else
      (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (not (empty? a-seq))
      (cons (seq a-seq) (tails (rest a-seq)))
    :else
      (cons () (seq a-seq))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (seq a-seq)
    (map
     (fn [n _]
       (concat (drop n a-seq) (take n a-seq)))
     (iterate inc 0) a-seq)
    (list ())))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (first a-seq)
                        (if (contains? freqs (first a-seq))
                          (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                          (assoc freqs (first a-seq) 1))
                        freqs)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (empty? coll)
    ()
    (if (= n 0)
      ()
      (cons (first coll) (my-take (- n 1) (rest coll))))))

(defn my-drop [n coll]
  (if (empty? coll)
    ()
    (if (not (= n 0))
      (my-drop (- n 1) (rest coll))
      (cons (first coll) (my-drop n (rest coll))))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        first-half (my-take half a-seq)
        second-half (my-drop half a-seq)]
    [first-half, second-half]))

(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq)))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    ()
    (if (< (count a-seq) 2)
      a-seq
      (let [first-half (first (halve a-seq))
            second-half (second (halve a-seq))
            first-sorted (sort first-half)
            second-sorted (sort second-half)]
        (seq-merge first-sorted second-sorted)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

