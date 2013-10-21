(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp power]
                 (cond
                  (zero? base) 0
                  (zero? exp) power
                  :else (recur base (dec exp) (* power base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (not (= (count seq1) (count seq2))) false
   :else (loop [sequ1 seq1
                sequ2 seq2]
           (cond
            (and (empty? sequ1) (empty? sequ2)) true
            (not (= (first sequ1) (first sequ2))) false
            :else (recur (rest sequ1) (rest sequ2))))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) index
     :else (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         amount 0
         a-seq a-seq]
    (cond
     (and (empty? a-seq) (zero? amount)) 0
     (empty? a-seq) (/ sum amount)
     :else (recur (+ sum (first a-seq)) (inc amount) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-elements #{}
         a-seq a-seq]
    (if (empty? a-seq)
      odd-elements
      (recur (toggle odd-elements (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [fn0 0
         fn-1 1
         index n]
    (if (zero? index)
      fn0
      (recur fn-1 (+ fn0 fn-1) (dec index)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         past []]
    (cond
     (empty? a-seq) past
     (contains? (set past) (first a-seq)) past
     :else (recur (rest a-seq) (conj past (first a-seq))))))
