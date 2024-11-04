;;;;Function to get time

(print (multiple-value-bind (s m h) (get-decoded-time)
  (format nil "Hours ~A Minutes ~A Seconds ~A" h m s)))
