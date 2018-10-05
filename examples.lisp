(in-package #:cl-user)

(defun simple-notepad-3-1 ()
  (iup:with-iup
    (let* ((text (iup:multi-line :expand "YES"))
	   (vbox (iup:vbox (list text)))
	   (dlg (iup:dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER")))
      (iup:show-xy dlg iup:+center+ iup:+center+)
      (setf (iup:attribute dlg :usersize) nil)
      (iup:main-loop))))


#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-notepad-3-1))

#+nil
(defun simple-web-browser ()
  (with-iup
    (image-lib-open)
    (iup-web:web-browser-open)
    (let* ((web-browser (iup-web:web-browser))
	   (vbox (vbox (list web-browser)))
	   (dlg (dialog vbox :size "HALFxHALF")))
      (show-xy dlg +center+ +center+)
      (main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-web-browser))
