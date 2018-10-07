(in-package #:cl-user)

(iup:actio)
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (iup:with-iup ()
    (let* ((dialog (iup:dialog
		    (iup:hbox (list (iup:button :title "Button 1")
				    (iup:button :title "Button 2"))))))
      (iup:show dialog)
      (iup:main-loop))))
