(defn die
  "n - кол-во человек, m - счет, pos - позиция негра"
  [n m pos]
  (letfn [(helper [n pos start]
                  (if (> n 1)
                    (let [m' (if (> m n) (mod m n) m)
                          to (- (mod (+ start m') n) 1)
                          stop (cond
                                 (= 0 to) n
                                 (< to 0) (+ n to)
                                 :else to)]
                      (when (not= stop pos)
                        (helper (- n 1)
                                (if (< pos stop)
                                  pos
                                  (- pos 1))
                                stop)))
                    1))]
    (reduce #(let [coll %
                   start (+ %2 1)]
              (if (helper n pos start)
                (conj coll start)
                coll))
            #{}
            (range n))))
