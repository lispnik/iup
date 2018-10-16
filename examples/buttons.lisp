(in-package #:iup-examples.buttons)

;;; Demonstrate setting a button action at creation time (via keyword paremeter
;;; and at post creation, using (setf callback).

(cffi:defcallback button-cb :int ((handle iup-cffi::ihandle))
  (iup:message "Button Callback " (format nil "Callback from ~A" (iup:attribute handle :title)))
  iup::+default+)

(defun buttons ()
  (iup:with-iup ()
    (let* ((button1 (iup:button :title "Button 1"))
	   (button2 (iup:button :title "Button 2" :action 'button-cb))
	   (dialog (iup:dialog (iup:hbox (list button1 button2) :gap 5 :margin "5x5")
			       :title "IupButtons")))
      (setf (iup:callback button1 :action) 'button-cb)
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (buttons))
