(in-package #:iup-dialogs)

(cffi:defcallback file-dialog-cb :int ((handle iup-cffi::ihandle))
  (let ((dialog (iup:file-dialog)))
    (unwind-protect
	 (iup:popup dialog  iup:+center+ iup:+center+)
      (iup:destroy dialog)))
  iup::+default+)

(cffi:defcallback message-dialog-cb :int ((handle iup-cffi::ihandle))
  (iup:popup (iup:message-dialog) iup:+center+ iup:+center+)
  iup::+default+)

(cffi:defcallback file-dialog-cb :int ((handle iup-cffi::ihandle))
  (iup:popup (iup:file-dialog) iup:+center+ iup:+center+)
  iup::+default+)

(defun dialogs ()
  (iup:with-iup ()
    (let* ((buttons (list (iup:button :title "IupFileDlg"
				      :expand "HORIZONTAL"
				      :action (cffi:get-callback 'file-dialog-cb))
			  (iup:button :title "IupMessageDlg"
				      :expand "HORIZONTAL"
				      :action (cffi:get-callback 'message-dialog-cb))
			  (iup:button :title "IupColorDlg" :expand "HORIZONTAL")
			  (iup:button :title "IupFontDlg" :expand "HORIZONTAL")
			  (iup:button :title "IupProgressDlg" :expand "HORIZONTAL")
			  (iup:button :title "IupScintillaDlg" :expand "HORIZONTAL"))
		    )
	   (vbox (iup:vbox buttons ))
	   (dialog (iup:dialog vbox :title "Iup Predefined Dialogs")))
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (dialogs))

