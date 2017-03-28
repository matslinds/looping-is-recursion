 (ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (== 0 exp)
                    acc
                    (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
    (let [helper (fn [aseq bseq]
                   (if (and (empty? aseq) (empty? bseq))
                            true
                           (if (= (first aseq) (first bseq))
                             (recur (rest aseq) (rest bseq))
                             false
                     )))]
      (if (== (count seq1) (count seq2))
        (helper seq1 seq2)
        false)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seqq a-seq]
    (if (empty? seqq)
      nil
      (if (pred (first seqq))
        index
        (recur (inc index) (rest seqq))))))

(defn avg [a-seq]
  (loop [nth 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (if (== nth 0)
        0
        (/ sum nth))
      (recur (inc nth) (+ sum (first seq)) (rest seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
                                  (disj a-set elem)
                                  (conj a-set elem)))]
  (loop [seq a-seq
         set #{}]
    (if (empty? seq)
      set
      (recur (rest seq) (toggle set (first seq)))))))

(defn fast-fibo [n]
  (if (== n 0)
    0
    (loop [nth n
           F0 0
           F1 1]
      (if (== nth 1)
        F1
        (recur (dec nth) F1 (+ F0 F1))))))

(defn cut-at-repetition [a-seq]
  (let [help (fn [ele sqnc]
               (loop [v ele
                      seq sqnc]
                 (if (empty? seq)
                   false
                   (if (= v (first seq))
                     true
                     (recur v (rest seq))))))]
    (loop [elem (first a-seq)
           seq1 a-seq
           seq2 []]
      (if (empty? seq1)
        seq2
        (if (help elem seq2)
          seq2
          (recur (first (rest seq1)) (rest seq1) (conj seq2 elem)))))))
