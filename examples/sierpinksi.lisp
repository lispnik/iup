#-sbcl (sierpinski)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (sierpinski))
