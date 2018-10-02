(in-package #:iup-cffi)

(defun example-2-1 ()
  (unwind-protect
       (progn 
	 (%iup-open (cffi:null-pointer) (cffi:null-pointer))
	 (%iup-message "title" (format nil "IUP version: ~A" (%iup-version))))
    (%iup-close)))

(defun example-2-2 ()
  (unwind-protect
       (progn
	 (%iup-open (cffi:null-pointer) (cffi:null-pointer))
	 (let* ((label (%iup-label "Hello world from IUP"))
		(dlg (%iup-dialog (%iup-vbox label))))
	   (%iup-set-str-attribute dlg "TITLE" "Hello World 2")
	   (%iup-show-xy dlg +IUP-CENTER+ +IUP-CENTER+)
	   (%iup-main-loop)))
    (%iup-close)))

(unwind-protect
     (progn
       (%iup-open (cffi:null-pointer) (cffi:null-pointer))
       (%iup-image-lib-open))
  (%iup-close))

(in-package #:iup)

(with-iup
  (let ((multi-text (multi-text )))))
