(in-package #:iup)

(export '(platform platform-string call-with-iup with-iup))

(defun call-with-iup (func)
  #+windows (iup-cffi::%set-process-dpi-aware)
  (iup:open)
  (unwind-protect
       (progn
	 (funcall func))
    (iup:close)
    (iup::unregister-all-callbacks)))

(defmacro with-iup (() &body body)
  `(call-with-iup #'(lambda () ,@body)))

(defun platform ()
  (cl:list :iup-version (iup:version)
           :iup-version-date (iup:version-date)
           :driver (iup:global :driver)
           #+linux :gtk-version #+linux (iup:global :gtkversion)
           :system (iup:global :system)
           :system-version (iup:global :systemversion)
           :lisp-implementation-type (lisp-implementation-type)
           :lisp-implementation-version (lisp-implementation-version)))

(defun platform-string ()
  (destructuring-bind
      (&key iup-version iup-version-date driver gtk-version system system-version lisp-implementation-type lisp-implementation-version)
      (platform)
    (format nil "IUP ~A, ~A ~A~@[ ~A~] on ~A ~A, ~A ~A"
            iup-version iup-version-date
            driver
            gtk-version
            system
            system-version
            lisp-implementation-type
            lisp-implementation-version)))
