(in-package #:iup-web-browser)

;;; webbrowser.c

(defun web-browser-test ()
	      
  )
	    
(defun web-browser (&rest files)
  (iup:with-iup
    (iup-web-browser::opene)
    (iup:image-lib-open)
    (let* ((config (iup:config :app_name "web_browser")))
      (iup:config-load config)
      (let* ((dialog (create-main-dialog config)))
	(iup:config-dialog-show config dialog "MainWindow")
	(new-file dialog)
	(dolist (file files)
	  (open-file dialog file))
	(iup:main-loop)))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (simple-notepad))

