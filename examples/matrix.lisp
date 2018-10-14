(in-package #:iup-matrix)

(defun create-matrix ()
  (let ((matrix (iup-controls:matrix
		 :numlin 20
		 :numcol 8)))
    (setf (iup:attribute-id-2 matrix :|| 1 1) (format nil "5.6~%3.33"))
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
