(defstruct movie
  title director year type)

(defun add-movie (movie array)
  (if
   (progn (dotimes (index (length array))
     (if (and (typep (aref array index) 'movie)
              (equal (movie-title (aref array index)) (movie-title movie)))
         (return t))))
   nil
   (progn
     (dotimes (index (length array))
     (if (null (aref array index))
         (progn
           (setf (aref array index) movie)
           (return array))
         ))
     )))

(defun delete-movie (title array)
  (dotimes (index (length array))
    (if (and (typep (aref array index) 'movie)
             (equal (movie-title (aref array index)) title))
        (progn
          (setf (aref array index) nil)
          (return array))))
  )

(defun num-movies (array)
  (let ((sum 0))
    (dotimes (index (length array) sum)
      (if (typep (aref array index) 'movie)
          (incf sum 1))
      )))
