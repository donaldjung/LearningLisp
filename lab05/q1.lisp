(defstruct STACK
  (elements (list))
  )

(defun create-stack ()
  (make-stack)
  )

(defun stack-push (stack element)
  (setf (stack-elements stack) (cons element (stack-elements stack)))  
  stack
  )

(defun stack-pop (stack)
  (let ((element (first (stack-elements stack))))
       (setf (stack-elements stack) (rest (stack-elements stack)))
       element
    )
  )

(defun stack-top (stack)
  (first (stack-elements stack))
  )

(defun stack-empty? (stack)
  (if (null (stack-elements stack)) t nil)
  )
