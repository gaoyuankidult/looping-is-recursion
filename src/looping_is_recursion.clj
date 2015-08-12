(ns looping-is-recursion)

(defn power [number pow]
  (let [helper (fn [result number pow]
                  (if (zero? pow)
                    result
                    (recur (* result number) number (- pow 1))))]
    (helper 1 number pow)))

(defn last-element [a-seq]
  (let [helper (fn [last-ele a-seq]
                 (if (empty? a-seq)
                   last-ele
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [set-1 set-2]
  (let [helper (fn [set-1 set-2]
                  (if (or (empty? set-1) (empty? set-2))
                    (if (and (empty? set-1) (empty? set-2))
		    	true
                        false)

		   (if (= (first set-1) (first set-2))
                    (recur (rest set-1) (rest set-2))
		    false)
                    ))]
    (helper (seq set-1) (seq set-2))))

;;(defn seq= [set-1 set-2]
;;[set-1 set-2]
;;)

(defn find-first-index [pred a-seq]
  (let [helper (fn [n pred a-seq]
       	       (if (empty? a-seq)
	       	   nil
                   (if (pred (first a-seq))
                       n
                       (if (empty? (rest a-seq))
                       	   nil
                           (recur (+ 1 n) pred (rest a-seq)))
                    	   )))]
    (helper 0 pred a-seq)))

(defn avg [a-seq]
  (let [helper (fn [sum a-seq]
                 (if (empty? a-seq)
                   sum
                   (recur (+ sum (first a-seq)) (rest a-seq))))]
    (/ (helper 0 a-seq) (count a-seq))))


(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  (let [helper (fn [n s-1 s-2]
                 (if (zero? n)
                   s-1
                   (recur (- n 1) s-2 (+ s-1 s-2))
                   ))]
    (helper n 0 1)))

(defn toggle [s e]
  (if (contains? s e) (disj s e) (conj s e))
  )

(defn cut-at-repetition [a-seq]
  (let [helper (fn [a-len a-set r-seq a-seq]
                 (if (= a-len (+ (count (toggle a-set (first a-seq))) (count (rest a-seq))))
                   (recur a-len	(toggle a-set (first a-seq)) (cons (first a-seq) r-seq) (rest a-seq))
                   (reverse r-seq)
                   ))]
    (helper (count a-seq) #{} () a-seq)))

(defn parity [a-seq]
      (let [helper (fn [a-set a-seq]
      	   	       (if (empty? a-seq)
		       	   a-set
			   (recur (toggle a-set (first a-seq)) (rest a-seq))))]
	(helper #{} a-seq)))

      



