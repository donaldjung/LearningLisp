(defun is-palindrome? (list1)
  (let ((len (floor (length list1) 2)))
    (equal (subseq list1 0 len) (reverse (subseq list1 (- (length list1) len))))
    
    ))

(defun prin-squares-of-3s (y)
  (do* ((num 1 (* num 3)))
      ((< y (* num num)) num)
    (print (* num num))
    )
  )

(defun array-stats (arr)
 null)

(defun generate-list (start end)
  (let ((list1 ()))
    (dotimes (num (- (+ end 1) start) list1)
      (setf list1 (cons (+ start num) list1))
      (print list1)
      )
    (reverse list1)
    )
  )

(defun generate-list-rec (start end)
  (if (equal start (+ end 1)) ()
      (cons start (generate-list-rec (+ start 1) end))))

(defun tgenerate-list-rec (start end &optional (list1 ()))
  (if (equal start (+ end 1)) (reverse list1)
      (tgenerate-list-rec (+ start 1) end (cons start list1)))
  )

(defun list-slice (list1 start end)
  (if (equal start (- end 1)) ()
      (cons (nth start list1) (list-slice list1 (+ 1 start) end)))
  )

(defun discount-calculator (x)
  (cond
    ((<= x 0) "Invalid Price")
    ((> x 100) (* x 0.85))
    ((< x 20) (* x 0.95))
    ((>= x 20) (* x 0.9))
    )
  )

(defun remove-int-duplicates (list1)
  (cond
    ((null list1) ())
    ((member (car list1) (rest list1)) (remove-int-duplicates2 (rest list1)))
    (t (cons (car list1) (remove-int-duplicates2
                          (rest list1)))))
  )

(defun remove-int-duplicates-tail (list1 &optional (acc ()))
  (cond
    ((null list1) (reverse acc))
    ((member (car list1) acc) (remove-int-duplicates-tail (rest list1) acc))
    (t (remove-int-duplicates-tail (rest list1) (cons (first list1) acc)))
    ))

(defun general-comparator (x y comp)
  (cond
    ((= 0 (funcall comp x y)) "The values are equal")
    ((< 0 (funcall comp x y)) "The first value is greater than the second")
    ((> 0 (funcall comp x y)) "The second value is greater than the second")
    )
  )

(defun func (x y)
  (cond
    ((= x y) 0)
    ((> x y) 1)
    ((< x y) -1)
    ))

(defun sum-deep-list (list1 &optional (sum 0))
  (cond
    ((null list1) sum)
    ((typep (first list1) 'list) (sum-deep-list (append (first list1) (rest list1)) sum))
    ((typep (first list1) 'integer) (sum-deep-list (rest list1) (+ sum (first list1))))
    (t (sum-deep-list (rest list1) sum))
    )
  )

(defun flatten-list-tail (list1 &optional (acc ()))
  (cond
    ((null list1) (reverse acc))
    ((typep (first list1) 'list) (flatten-list (append (first list1) (rest list1)) acc))
    (t (flatten-list (rest list1) (cons (first list1) acc)))

    )
  )

(defun flatten-list (list1)
  (cond
    ((null list1) ())
    ((typep (first list1) 'list) (append (first list1) (flatten-list (rest list1))))
    (t (cons (first list1) (flatten-list (rest list1))))
    ))

(defun average-1 (list1)
  (let
      ((sum 0))
    (dolist (item list1 sum)
      (setf sum (+ sum item))
      )
    (/ sum (length list1))
    ))

(defun sum-of-natural-nums (n &optional (sum 0))
  (if (= n 0) sum
      (sum-of-natural-nums (- n 1) (+ sum n)))
  )

(defun factorial (n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))
      ))

(defun fact-tail (n &optional (sum 1))
  (if (= n 0) sum
      (fact-tail (- n 1) (* sum n)))
  )

(defun fact-list (list1)
  (if (null list1) 1
      (* (first list1) (fact-list (rest list1))))
  )

(defun max-element (list1)
  (cond
    ((null (rest list1)) (first list1)) 
    ((> (first list1) (second list1)) (append (first list1) (rest (rest list1))))
    ((< (first list1) (second list1) ()))

      )
  )

(defun max-element-tail (list1 &optional (max 0))
  (cond
    ((null list1) max)
    ((< max (first list1)) (max-element-tail (rest list1) (first list1)))
    (t (max-element-tail (rest list1) max))
    )
  )

(defun factorial-list (list1)
  (cond
    ((null list1) ())
    (t (cons (factorial (first list1)) (factorial-list (rest list1))))
    )
  )

(defun factorial-list-tail (list1 &optional (list2 ()))
  (cond
    ((null list1) (reverse list2))
    (t (factorial-list-tail (rest list1) (cons (factorial (first list1)) list2)))
    )
  )

(defun remove-dup2 (list1)
  (cond
    ((null list1) ())
    ((member (first list1) (rest list1)) (remove-dup2 (rest list1)))
    (t (cons (first list1) (remove-dup2 (rest list1))))
    ))

(defun only-evens (list1)
  (cond
    ((null list1) ())
    ((oddp (first list1)) (only-evens (rest list1)))
    (t (cons (first list1) (only-evens (rest list1))))
      ))

(defun merge-sorted-lists (list1 list2 comp)
  (cond
    ((and (null list1) (null list2)) ())
    
    ((null list1) (cons (first list2) (merge-sorted-lists list1 (rest list2) comp)))
    ((null list2) (cons (first list1) (merge-sorted-lists (rest list1) list2 comp)))
    ((funcall comp (first list1) (first list2)) (cons (first list1) (merge-sorted-lists (rest list1) list2 comp)))
    (t (cons (first list1) (merge-sorted-lists list1 (rest list2) comp)))
    )
  )
