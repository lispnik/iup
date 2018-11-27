(defpackage #:iup-examples.dialogs
  (:use #:common-lisp)
  (:export #:dialogs))

(in-package #:iup-examples.dialogs)

(defun file-dialog (handle)
  (declare (ignore handle))
  (let ((dialog (iup:file-dialog)))
    (unwind-protect
	 (iup:popup dialog iup:+center+ iup:+center+)
      (iup:destroy dialog)))
  iup:+default+)

(defun message-dialog (handle)
  (let ((dialog (iup:message-dialog 
		 :title "IupMessageDlg"
		 ;;		 :value "IupMessageDlg test"
		 :dialogtype "INFORMATION"
		 :buttons "YESNOCANCEL")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Got button response ~S" (iup:attribute dialog "BUTTONRESPONSE"))))
      (iup:destroy dialog)))
  iup:+default+)

(defun color-dialog (handle)
  (declare (ignore handle))
  (let ((dialog (iup:color-dialog
		 :title "IupColorDlg"
		 :showhex "YES"
		 :showcolortable "YES"
		 :showalpha "YES")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Got button response ~S~%Got color ~A RGB (~A HSI, ~A)"
					 (iup:attribute dialog :status)
					 (iup:attribute dialog :value)
					 (iup:attribute dialog :valuehsi)
					 (iup:attribute dialog :valuehex))))))
  iup:+default+)

(defun font-dialog (handle)
  (declare (ignore handle))
  (let ((dialog (iup:font-dialog :title "IupFontDlg")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Got button response ~S~%Got font ~S"
					 (iup:attribute dialog :status)
					 (iup:attribute dialog :value))))))
  iup:+default+)

(defun scintilla-dialog (handle)
  (declare (ignore handle))
  (let ((dialog (iup-scintilla:scintilla-dialog :title "IupScintillaDlg")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)))))

(defun dialogs ()
  (iup:with-iup ()
    (iup-scintilla:open)
    (flet ((button (title callback)
	     (iup:button :title title :action callback :expand "HORIZONTAL")))
      (let* ((dialog (iup:dialog
		      (iup:vbox (list (button "IupFileDlg" 'file-dialog)
				      (button "IupMessageDlg" 'message-dialog)
				      (button "IupColorDlg" 'color-dialog)
				      (button "IupFontDlg" 'font-dialog)
				      ;; (button "IupProgressDlg" nil)
				      (button "IupScintillaDlg" 'scintilla-dialog)))
		      :title "Iup Predefined Dialogs")))
	(iup:show-xy dialog iup:+center+ iup:+center+)
	(iup:main-loop)))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (dialogs))
