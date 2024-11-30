(defun interleave (list1 list2)
  (cond
    ((and (null list1) (null list2)) ())
    ((null list1) (cons (first list2) (interleave list1 (rest list2))))
    ((null list2) (cons (first list1) (interleave (rest list1) list2)))
    (t (cons (first list1) (interleave list2 (rest list1))))
    ))

(defun interleave-r (list1 list2 &optional (acc ()))
  (cond
    ((and (null list1) (null list2)) (reverse acc))
    ((null list1) (interleave-r list1 (rest list2) (cons (first list2) acc)))
    ((null list2) (interleave-r (rest list1) list2 (cons (first list1) acc)))
    (t (interleave-r list2 (rest list1) (cons (first list1) acc)))

    )
  )


(defun mapf (func list1)
  (cond
    ((null list1) ())
    (t (cons (funcall func (first list1)) (mapf func (rest list1)))))
  )

(defun mapf-r (func list1 &optional (acc ()))
  (cond
    ((null list1) (reverse acc))
    (t (mapf-r func (rest list1) (cons (funcall func (first list1)) acc)))


    )
  )

(defun comb (n k)
  (cond
    ((> 0 n) nil)
    ((> k n) nil)
    ((= k 0) 1)
    ((= n k) 1)
    (t (+ (comb (- n 1) k) (comb (- n 1) (- k 1))))
    )
  )

(defun fact (x)
  (if (= x 0) 1
      (* x (fact (- x 1)))))

