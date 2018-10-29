(ql:quickload "iup")
 (sb-int:with-float-traps-masked
	     (:divide-by-zero :invalid)
	   (iup:with-iup ()
	     (let* ((button (iup:button :title "Press Me"))
		    (hbox (iup:hbox (cl:list button)))
		    (dialog (iup:dialog hbox)))
	       (iup:show dialog)
	       (iup:main-loop))))
