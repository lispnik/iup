(in-package #:iup)

(defun simple-notepad-3-1 ()
  (with-iup
    (let* ((text (multi-line :expand "YES"))
	   (vbox (vbox text))
	   (dlg (dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER")))
      (show-xy dlg +center+ +center+)
      (setf (attr dlg :usersize) nil)
      (main-loop))))

(cffi:defcallback exit-cb :int ((handle iup-cffi::ihandle))
  (declare (ignore handle))
  -3)

(defun simple-notepad-3-2 ()
  (with-iup
    (let* ((text (multi-line :expand "YES"))
	   (item-open (item :title "Open"))
	   (item-save-as (item :title "Save As"))
	   (item-exit (item :title "Exit"))
	   (file-menu (menu item-open item-save-as (separator) item-exit))
	   (submenu (submenu file-menu :title "File"))
	   (menu (menu submenu))
	   (vbox (vbox text))
	   (dlg (dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER" :menu menu)))
;;      (iup-cffi::%iup-set-callback item-exit :action (callback exit-cb))
      (iup-cffi::%iup-set-callback item-exit :action (cffi:callback exit-cb))
      (show-xy dlg +center+ +center+)
      (setf (attr dlg :usersize) nil)
      (main-loop))))

(lambda (handle))
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-notepad-3-2))
