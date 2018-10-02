(in-package #:iup)

(defun simple-notepad ()
  (sb-int:with-float-traps-masked
      (:divide-by-zero)
    (with-iup
      (let* ((text (multi-line :expand "YES"))
	     (vbox (vbox text))
	     (dlg (dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER")))
	(show-xy dlg +center+ +center+)
	(setf (attr dlg :usersize) nil)
;;;	(print (attr dlg :usersize))
	(print (attr text :expand))
	(main-loop)))))

#+nil (simple-notepad)
