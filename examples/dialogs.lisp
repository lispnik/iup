(in-package #:iup-dialogs)

(cffi:defcallback file-dialog-cb :int ((handle iup-cffi::ihandle))
  (let ((dialog (iup:file-dialog)))
    (unwind-protect
	 (iup:popup dialog iup:+center+ iup:+center+)
      (iup:destroy dialog)))
  iup::+default+)

(cffi:defcallback message-dialog-cb :int ((handle iup-cffi::ihandle))
  (let ((dialog (iup:message-dialog
		 :title "IupMessageDlg"
		 :value "IupMessageDlg Example Text"
		 :dialogtype "INFORMATION"
		 :buttons "YESNOCANCEL")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Got button response ~S" (iup:attribute dialog :buttonresponse))))
      (iup:destroy dialog)))
  iup::+default+)

(cffi:defcallback color-dialog-cb :int ((handle iup-cffi::ihandle))
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

  iup::+default+)

(cffi:defcallback font-dialog-cb :int ((handle iup-cffi::ihandle))
  (let ((dialog (iup:font-dialog :title "IupFontDlg")))
    (unwind-protect
	 (progn
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Go button response ~S~%Got font ~S"
					 (iup:attribute dialog :status)
					 (iup:attribute dialog :value)))))))

(cffi:defcallback scintilla-dialog-cb :int ((handle iup-cffi::ihandle))
  (let ((dialog (iup-scintilla:scintilla-dialog :title "IupScintillaDlg")))
    (unwind-protect
	 (progn
	   (break)
	   (iup:popup dialog iup:+center+ iup:+center+)
	   (iup:message "Result" (format nil "Go button response ~S~%Got font ~S"
					 (iup:attribute dialog :status)
					 (iup:attribute dialog :value)))))))

(defun dialogs ()
  (iup:with-iup ()
    (iup-scintilla:open)
    (flet ((button (title callback)
	     (iup:button :title title :action callback :expand "HORIZONTAL")))
      (let* ((buttons (list (button "IupFileDlg" 'file-dialog-cb)
			    (button "IupMessageDlg" 'message-dialog-cb)
			    (button "IupColorDlg" 'color-dialog-cb)
			    (button "IupFontDlg" 'font-dialog-cb)
			    (button "IupProgressDlg" nil)
			    (button "IupScintillaDlg" 'scintilla-dialog-cb)))
	     (vbox (iup:vbox buttons))
	     (dialog (iup:dialog vbox :title "Iup Predefined Dialogs")))
	(iup:show dialog)
	(iup:main-loop)))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (dialogs))
