(defvar capacity1 0)
(defvar capacity2 0)
(defvar parked1 0)
(defvar parked2 0)

(defun max-capacity (b n)
  (if (= b 1)
      (progn
        (setf parked1 0)
        (setf capacity1 n))
      (progn
        (setf parked2 0)
        (setf capacity2 n))))

(defun enter-garage (b)
  (if (= b 1)
      (progn
        (if (> capacity1 parked1)
            (progn
              (setf parked1 (+ parked1 1))
              (- capacity1 parked1))
            (- capacity1 parked1)))
      (progn
        (if (> capacity2 parked2)
            (progn
              (setf parked2 (+ parked2 1))
              (- capacity2 parked2))
            (- capacity2 parked2)))))

(defun exit-garage (b)
  (if (= b 1)
      (progn
        (setf parked1 (- parked1 1))
        (- capacity1 parked1))
      (progn
        (setf parked2 (- parked2 1))
        (- capacity2 parked2))))
