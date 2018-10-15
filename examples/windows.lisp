(in-package #:iup-flashbar)

(cffi:define-foreign-library user32
  (:windows "user32.dll"))

(cffi:use-foreign-library user32)

(cffi:defcstruct flashwinfo
  (cbsize :uint)
  (hwnd :pointer)
  (dwflags :uint32)
  (ucount :uint)
  (dwtimeout :uint32))

(cffi:defcfun (flashwindowex "FlashWindowEx") :boolean (pfwi :pointer))

(defun flash (hwnd-in)
  (cffi:with-foreign-object (ptr 'flashwinfo)
    (cffi:with-foreign-slots ((cbsize hwnd dwflags ucount dwtimeout) ptr flashwinfo)
      (setf cbsize (cffi:foreign-type-size 'flashwinfo)
	    hwnd hwnd-in
	    dwflags (logior #x3 #xc)
	    ucount 10
	    dwtimeout 0)
      (print (flashwindowex ptr)))))

(defun flashbar ()
  (iup:with-iup ()
    (let* ((vbox (iup:vbox (list (iup:button :title "Click")) :margin "10x10"))
	   (dialog (iup:dialog vbox)))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (flash (iup:attribute dialog :hwnd :pointer))
      (iup:main-loop))))
