(defstruct point1
  x
  y)

(defstruct (point2 (:conc-name p))
  (x 0)
  (y 0))

(defstruct (node
 elt (1 nil) (r nil)))

