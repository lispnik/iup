(in-package #:iup-web-browser)

;;; webbrowser.c

(defun web-browser ()
  (iup:with-iup ()
    (iup-web:web-browser-open)
    (let* ((btn-back (iup:button :title "Back" :action nil))
	   (btn-forward (iup:button :title "Forward" :action nil))
	   (text (iup:text :expand "HORIZONTAL" :value "https://lisp-lang.org"))
	   (btn-load (iup:button :title "Load"))
	   (btn-reload (iup:button :title "Reload"))
	   (btn-stop (iup:button :title "Stop"))
	   #+linux (btn-history (iup:button :title "History"))
	   (web (iup-web:web-browser))
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
		    :gap "5")))
      (iup:show dialog)
      (iup:main-loop))))
	    
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (web-browser-test))

