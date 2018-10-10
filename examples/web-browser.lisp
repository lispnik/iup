(in-package #:iup-web-browser)

;;; webbrowser.c

(cffi:defcallback error-cb :int ((handle iup-cffi::ihandle) (url :string))
  (declare (ignore handle))
  (iup:message-error nil (format nil "Error: ~A" url))
  iup::+default+)

(cffi:defcallback newwindow-cb :int ((handle iup-cffi::ihandle) (url :string))
  (setf (iup:attribute handle :value) url)
  iup::+default+)

(cffi:defcallback back-cb :int ((handle iup-cffi::ihandle))
  (let ((web (iup:attribute handle :my_web)))
    (setf (iup:attribute web :backforward) "-1"))
  iup::+default+)

(cffi:defcallback forward-cb :int ((handle iup-cffi::ihandle))
  (let ((web (iup:attribute handle :my_web)))
    (setf (iup:attribute web :backforward) "1"))
  iup::+default+)

(cffi:defcallback stop-cb :int ((handle iup-cffi::ihandle))
  (let ((web (iup:attribute handle :my_web)))
      (setf (iup:attribute web :stop) nil))
  iup::+default+)

(cffi:defcallback reload-cb :int ((handle iup-cffi::ihandle))
  (let ((web (iup:attribute handle :my_web)))
      (setf (iup:attribute web :reload) nil))
  iup::+default+)

 (cffi:defcallback load-cb :int ((handle iup-cffi::ihandle))
  (let* ((txt (iup:attribute handle :my_txt))
 	 (web (iup:attribute handle :my_web)))
     (setf (iup:attribute web :value) (iup:attribute txt :value)))
   iup::+default+)
 
#+linux
(cffi::defcallback history-cb :int ((handle iup-cffi::ihandle))
  (let ((back (parse-integer (iup:attribute handle :backcount)))
	(fwrd (parse-integer (iup:attribute handle :forwardcount))))
    (loop for i from (- back) below 0 
	  do (format t "Backaward ~A ~A" i (iup:attribute handle (format nil "ITEMHISTORY~A" i))))
    (format t "Current: ~A" (iup:attribute handle :itemhistory0))
    (loop for i from 0 to fwrd
	  do (format t "Forward ~A ~A" i (iup:attribute handle (format nil "ITEMHISTORY~A" i))))))

(defun web-browser ()
  (iup:with-iup ()
    (iup-web:web-browser-open)
    (let* ((btn-back (iup:button :title "Back" :action 'back-cb))
	   (btn-forward (iup:button :title "Forward" :action 'forward-cb))
	   (text (iup:text :expand "HORIZONTAL" :value "https://lisp-lang.org"))
	   (btn-load (iup:button :title "Load" :action 'load-cb))
	   (btn-reload (iup:button :title "Reload" :action 'reload-cb))
	   (btn-stop (iup:button :title "Stop" :action 'stop-cb))
	   #+linux (btn-history (iup:button :title "History" :action 'history-cb))
	   (web (iup-web:web-browser :expand "YES"))
	   (dialog (iup:dialog
		    (iup:vbox
		     (list (iup:hbox (list  btn-back
					    btn-forward
					    text
					    btn-load
					    btn-reload
					    btn-stop
					    #+linux btn-history))
			   web))
		    :title "IupWebBrowser"
		    :rastersize "800x600"
		    :margin "5x5"
		    :gap "5"
		    :my_txt text
		    :my_web web)))
      (iup:show dialog)
      (iup:main-loop))))
	    
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (web-browser))

