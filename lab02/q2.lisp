;; Type this code, as is, in your lab02-ans.lisp file.
;; DO NOT MODIFY THIS CODE
(defparameter *events* (list))

(defun roll-dice ()
  (let* ((s1 (1+ (random 6)))
         (s2 (1+ (random 6)))
         (sum (+ s1 s2)))
    (push (list sum s1 s2) *events*)
    sum))

(defun execute-experiment (n)
  "This function implements a simple interface for analyzing the
   operation of function SIMULATE-DICE-ROLLS"
  (setf *events* (list))
  (let ((res (simulate-dice-rolls n)))
    (format t "simulate-dice-rolls returned: ~a~%(Sum Dice1 Dice2) values: ~a~%" res (reverse *events*))))

(defun simulate-dice-rolls (n)
  (let ((previous 0)
        (next 0)
        (sum 0))
    (setf previous (roll-dice))
    (dotimes (index (- n 1) sum)
      (setf next (roll-dice))
      (if (> next previous)
          (incf sum 1))
      (setf previous next)
      )
    )
  )


