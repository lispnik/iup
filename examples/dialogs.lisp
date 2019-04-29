(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-scintilla")))

(defpackage #:iup-examples.dialogs
  (:use #:common-lisp)
  (:export #:dialogs))

(in-package #:iup-examples.dialogs)

(defun dialogs ()
  (iup:with-iup ()
    (iup-scintilla:open)
    (flet ((button (title callback)
	     (iup:button :title title
			 :action callback
			 :expand :horizontal)))
      (let* ((dialog (iup:dialog
		      (iup:vbox (list (button "File Dialog" 'file-dialog)
				      (button "Message Dialog" 'message-dialog)
				      (button "Color Dialog" 'color-dialog)
				      (button "Font Dialog" 'font-dialog)
				      (button "Scintilla Dialog" 'scintilla-dialog)
				      (button "Layout Dialog" 'layout-dialog)))
		      :title "IUP Predefined Dialogs")))
	(iup:show dialog)
	(iup:main-loop)))))

(defun file-dialog (handle)
  (let ((dialog (iup:file-dialog)))
    (unwind-protect
         (progn
           (iup:popup dialog iup:+center+ iup:+center+)
           (iup:message "File Dialog Example"
                        (format nil "Selected ~A" (iup:attribute dialog :value))))
      (iup:destroy dialog)))
  iup:+default+)

(defun message-dialog (handle)
  (let ((dialog (iup:message-dialog 
                 :dialogtype :warning
                 :buttons :retrycancel)))
    (unwind-protect
         (progn
           (setf (iup:attribute dialog :value) "Heap exhausted, game over.")
           (iup:popup dialog iup:+center+ iup:+center+)
           (iup:message "Message Dialog"
                        (format nil "Got button response ~S"
                                (iup:attribute dialog :buttonresponse))))
      (iup:destroy dialog)))
  iup:+default+)

(defun color-dialog (handle)
  (let ((dialog (iup:color-dialog
                 :title "IUP Color Dialog"
                 :showhex "YES"
                 :showcolortable "YES"
                 :showalpha "YES")))
    (unwind-protect
         (progn
           (iup:popup dialog iup:+center+ iup:+center+)
           (iup:message "Result"
                        (format nil "Got button response ~S~%Got color ~A RGB (~A HSI, ~A)"
                                (iup:attribute dialog :status)
                                (iup:attribute dialog :value)
                                (iup:attribute dialog :valuehsi)
                                (iup:attribute dialog :valuehex))))))
  iup:+default+)

(defun font-dialog (handle)
  (let ((dialog (iup:font-dialog :title "IUP Font Dialog")))
    (unwind-protect
         (progn
           (iup:popup dialog iup:+center+ iup:+center+)
           (iup:message "Result"
                        (format nil "Got button response ~S~%Got font ~S"
                                (iup:attribute dialog :status)
                                (iup:attribute dialog :value))))
      (iup:destroy dialog)))
  iup:+default+)

(defun scintilla-dialog (handle)
  (let ((dialog (iup-scintilla:scintilla-dialog :title "IUP Scintilla Dialog")))
    (unwind-protect
	 (iup:popup dialog iup:+center+ iup:+center+)
      (iup:destroy dialog))))

(defun layout-dialog (handle)
  (let ((dialog (iup:layout-dialog nil)))
    (unwind-protect
	 (iup:popup dialog iup:+center+ iup:+center+)
      (iup:destroy dialog)))
  iup:+default+)









#-sbcl (dialogs)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (dialogs))
