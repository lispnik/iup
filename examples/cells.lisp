(in-package #:iup-cells)

(defun cells ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((vbox (iup:vbox (list)))
	   (dialog (iup:dialog vbox :title "IUP Cells Test" :rastersize "800x600")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cells))
