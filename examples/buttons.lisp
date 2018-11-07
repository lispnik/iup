(defpackage #:iup-examples.buttons
  (:use #:common-lisp)
  (:export #:buttons))

(in-package #:iup-examples.buttons)

(defun button-cb (&rest rest)
  (iup:message "Test" (format nil "~A on ~A" (lisp-implementation-type) (cl:lisp-implementation-version)))
  iup::+default+)

(defun buttons ()
  (iup:with-iup ()
    (let* ((button1 (iup:button :title "Button 1"))
	   (button2 (iup:button :title "Button 2" :action 'button-cb))
	   (dialog (iup:dialog (iup:hbox (list button1 button2) :gap 5 :margin "5x5")
			       :title "IupButtons")))
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (buttons))
