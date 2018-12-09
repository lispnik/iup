(defpackage #:iup-examples.web-browser
  (:use #:common-lisp
	#:alexandria)
  (:export #:web-browser))

(in-package #:iup-examples.web-browser)

;;; webbrowser.c

(defvar *web* nil)
(defvar *url* "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")

(defun back (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :backforward) -1)
  iup:+default+)

(defun forward (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :backforward) 1)
  iup:+default+)

(defun stop (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :stop) nil)
  iup:+default+)

(defun reload (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :reload) nil)
  iup:+default+)

(defun goto (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :value) *url*)
  iup:+default+)

#+nil
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
    (iup-web:open)
    (let* ((btn-back (iup:button :title "Back" :action 'back))
	   (btn-forward (iup:button :title "Forward" :action 'forward))
	   (text (iup:text :expand "HORIZONTAL" :value *url*))
	   (btn-load (iup:button :title "Load" :action 'goto))
	   (btn-reload (iup:button :title "Reload" :action 'reload))
	   (btn-stop (iup:button :title "Stop" :action 'stop))
	   #+nil (btn-history (iup:button :title "History" :action 'history-cb))
	   (web (iup-web:web-browser :expand "YES"))
	   (dialog (iup:dialog 
		    (iup:vbox
		     (list (iup:hbox
			    (list btn-back
				  btn-forward
				  text
				  btn-load
				  btn-reload
				  btn-stop
				  #+nil btn-history
				  )
			    :margin "5x5"
			    :gap "5")
			   web))
		    :title "IupWebBrowser"
		    :rastersize "800x600"
		    :map_cb #'(lambda (handle)
				(declare (ignore handle))
				(setf *web* web)
				(goto nil)
				iup:+default+))))
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (web-browser))

