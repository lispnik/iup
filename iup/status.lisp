(in-package #:iup)

;;; macros from iup.h

(defun is-shift-p (status) (char= (char status 0) #\S))
(defun is-control-p (status) (char= (char status 1) #\C))

(defun is-button1-p (status) (char= (char status 2) #\1))
(defun is-button2-p (status) (char= (char status 3) #\2))
(defun is-button3-p (status) (char= (char status 4) #\3))

(defun is-double-p (status) (char= (char status 5) #\D))
(defun is-alt-p (status) (char= (char status 6) #\A))
(defun is-sys-p (status) (char= (char status 7) #\Y))

(defun is-button4-p (status) (char= (char status 8) #\4))
(defun is-button5-p (status) (char= (char status 9) #\5))

(defun status-plist (status)
  (cl:list
   :shift (is-shift-p status)
   :control (is-control-p status)
   :button1 (is-button1-p status)
   :button2 (is-button2-p status)
   :button3 (is-button3-p status)
   :button4 (is-button4-p status)
   :button5 (is-button5-p status)
   :double (is-double-p status)
   :alt (is-alt-p status)
   :sys (is-sys-p status)))

(export '(is-shift-p
	  is-control-p
	  is-button1-p
	  is-button2-p
	  is-button3-p
	  is-button4-p
	  is-button5-p
	  is-double-p
	  is-alt-p
	  is-sys-p
	  status-plist))
