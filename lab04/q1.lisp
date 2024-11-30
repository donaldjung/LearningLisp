(defstruct RECORD
  name
  score)

(defun sort-student-records (records func)
  (let ((new-list nil) (position 0) (counter 0))
    (dolist (student records new-list)
      (setf position 1)
      (dolist (student2 new-list position)
        (unless (funcall func student student2)
            (return position))
        (incf position)
        )
      (if (>= position (length new-list))
          (setf new-list (append new-list (list student)))
          (progn
            (setf new-list (append (subseq new-list 0 position)
                                   (list student)
                                   (subseq new-list position)))
            ))
      
      )
    (reverse new-list)
    )
  )





(defun sort-student-records2 (records func)
  (let ((new-list nil)
        (counter 0))
    (dolist (student records new-list)
      (setf counter 1)
      (dolist (student2 new-list)
        (if (funcall func student student2)
            (progn
              (setf new-list (append (subseq new-list 0 counter)
                                     (list student)
                                     (subseq new-list counter)))
              (return))
            )
        (incf counter)
        )
      (setf new-list (append new-list (list student)))
      )
    )
  )
