(in-package #:iup-examples.matrix)

(defun create-matrix ()
  (let ((matrix (iup-controls:matrix
		 :numlin 20
		 :numcol 8)))
    (setf (iup:attribute-id-2 matrix :|| 1 1) "5.6"
	  (iup:attribute-id-2 matrix :|| 2 1) 2.2 ;FIXME single floats not setting...
	  (iup:attribute-id-2 matrix :|| 3 2) "Very Very Very Very Very Large Text"
	  (iup:attribute-id-2 matrix :|| 1 2) 4
	  )
    
    matrix))

(defun matrix ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((vbox (iup:vbox (list (create-matrix))))
	   (dialog (iup:dialog vbox :title "IUP Matrix Test" :rastersize "800x600")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (matrix))
