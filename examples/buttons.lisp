(in-package #:iup-buttons)

(cffi:defcallback button1-cb :int ((handle iup-cffi::ihandle))
  (iup:message "Button 1 Callback" (format nil "callback from ~A" (iup:attribute handle "TITLE")))
  iup::+default+)

(defun buttons ()
  (iup:with-iup ()
    (let* ((button1 (iup:button :title "Button 1"))
	   (button2 (iup:button :title "Button 2"))
	   (dialog (iup:dialog (iup:hbox (list button1 button2) :gap "5" :margin "5x5")
			       :title "IupButtons")))
      (setf (iup:callback button1 :action) 'button1-cb)
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (buttons))

