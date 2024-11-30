(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
square brackets, arithmetic operations, and numbers."
  (let ((ops ())
        (vals ())
        (op nil)
        (val nil)
        (check 0))
    (dolist (item expr)
      (case item
        ([ )                            ; do nothing
        ((+ - * / ^ SDIV MAXF FACT) (push item ops))
        (] (setf op (pop ops) val (pop vals))
         (case op
           (+ (setf val (+ val (pop vals))))
           (- (setf val (- (pop vals)  val)))
           (* (setf val (* val (pop vals))))
           (/ (setf val (/ (pop vals)  val)))
           (^ (setf val (power (pop vals) val)))
           (SDIV (setf val (std (pop vals) (pop vals) val)))
           (MAXF (setf val (max (pop vals) (pop vals) val)))
           (FACT (setf val (fact val))))
         (push val vals))
        (otherwise (push item vals)))
      (case item
        ([ (incf check))
        (] (decf check))))
    (if (= check 0) (pop vals) nil))
  )



(defun power (x exp)
  (case exp
    (0 1)
    (1 x)
    (otherwise (let ((sum x))
    (dotimes (i (- exp 1))
      (setf sum (* x sum)))
    sum
    ))
    )
  )

(defun std (y x z)
  (/ (- x y) z)
)

(defun maxf (y x z)
  (cond 
     ((and (> x y) (> x z)) x)
     ((and (> y x) (> y z)) y)
     ((and (> z x) (> z y)) z)
      )
  )

(defun fact (x)
  (if (= x 1) 1
      (* x (fact (- x 1)))))
