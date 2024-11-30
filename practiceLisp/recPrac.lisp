(defun fact (x)
  (if (= x 1) 1
      (* x (fact (- x 1)))))

(defun fact-rec (x &optional (sum 1))
  (if (= x 1) sum
      (fact-rec (- x 1) (* x sum)))
  )

(defun sum-rec (list1)
  (if (null list1) 0
      (+ (car list1) (sum-rec (rest list1))))
  )

(defun sum-tail (list1 &optional (sum 0))
  (if (null list1) sum
      (sum-tail (rest list1) (+ sum (car list1))))
  )

(defun is-palindrome? (list1)
  ()

  )
