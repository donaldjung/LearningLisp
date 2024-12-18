(defun insertion-s (vec)

  (dotimes (i (- (length vec) 1) vec)
    (do ((j i (1- j)))
        ((< j 0) vec)
      (if (> (aref vec j) (aref vec (+ j 1)))
          (rotatef (aref vec j) (aref vec (+ j 1))))
      )
    )
  )

(defun selection-s (vec)
  (let ((low 0))
    (dotimes (i (- (length vec) 1) vec)
      (dotimes (j (- (length vec) i) vec)
        (if (< (aref vec (+ j i)) (aref vec low))
            (setf low (+ j i)))
      )
      (rotatef (aref vec i) (aref vec low))
    )
    )
  )


(defun binary-search (value vec &optional (position 0))
  (if (> (length vec) 1)
      (let* ((midpt (floor (length vec) 2))
             (midel (aref vec midpt)))
        (cond ((< midel value) (binary-search value (subseq vec midpt) (+ position midpt)))
              ((> midel value) (binary-search value (subseq vec 0 midpt) position))
              (t (+ midpt position))))
      (when (= (aref vec 0) value)
        position)))

(defun merge-lists (l1 l2)
   (let ((res ()))
     (do ()
         ((and (null l1) (null l2)))
       (let ((i1 (first l1))
             (i2 (first l2)))
         (cond
           ((null i1) (dolist (i l2) (push i res)) (return))
           ((null i2) (dolist (i l1) (push i res)) (return))
           ((< i1 i2) (push i1 res) (setf l1 (rest l1)))
           (t (push i2 res) (setf l2 (rest l2))))))
     (reverse res)))

(defun quicksort (vec comp)
(when (> (length vec) 1)
(let ((ppvt 0)
(pivot (aref vec (1- (length vec))))) ; last element
(dotimes (i (1- (length vec))) ; finds position of the pivot
(when (funcall comp (aref vec i) pivot)
(rotatef (aref vec i) (aref vec ppvt))
(incf ppvt)))
;; swap the pivot (last element) in its proper place
(rotatef (aref vec (1- (length vec))) (aref vec ppvt))
(quicksort (rtl:slice vec 0 ppvt) comp)
(quicksort (rtl:slice vec (1+ ppvt)) comp)))
vec)
