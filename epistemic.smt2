(declare-fun models (Int Bool) Bool)
(declare-fun K (Bool) Bool)
(declare-fun Rel (Int Int) Bool)

(assert (forall ((s Int) (p Bool))
    (= (models s (K p))
        (forall ((v Int)) (=> (and (<= 0 v) (< v 3)) (=> (Rel s v) (models v p)))))))
(assert (forall ((s Int) (p Bool))
    (= (models s p) p)))

; model

(assert (Rel 0 0))
(assert (Rel 0 1))
(assert (Rel 1 0))
(assert (Rel 1 1))
(assert (Rel 1 2))
(assert (Rel 2 1))
(assert (Rel 2 2))
(assert (not (Rel 0 2)))
(assert (not (Rel 2 0)))

(check-sat)
