(defpackage #:iup-cffi
  (:use #:common-lisp
	#:cffi))

(in-package #:iup-cffi)

(define-foreign-library iup
  (t (:default "iup")))

(use-foreign-library iup)

;;; FIXME implement returv value enums for this

(defcfun (%iup-open "IupOpen") :int
  (argv :pointer)
  (argc :pointer))

(defcfun (%iup-close "IupClose") :int) 

(defcfun (%iup-message "IupMessage") :void
  (title :string)
  (message :string))

(defcfun (%iup-version "IupVersion") :string)

(defctype ihandle :pointer)

(defcfun (%iup-label "IupLabel") ihandle
  (text :string))

(defcfun (%iup-set-str-attribute "IupSetStrAttribute") :void
  (handle ihandle)
  (name :string)
  (value :string))

(defcfun (%iup-set-str-attribute-id "IupSetStrAttributeId") :void
  (handle ihandle)
  (name :string)
  (id :int)
  (value :string))

(defcfun (%iup-set-str-attribute-id-2 "IupSetStrAttributeId2") :void
  (handle ihandle)
  (name :int)
  (line :int)
  (column :int)
  (value :string))

(defcfun (%iup-dialog "IupDialog") ihandle
  (child ihandle))

(defcfun (%%iup-vbox-v "IupVBoxv") ihandle
  (children :pointer))

(defun %iup-vbox (&rest children)
  (let ((array (foreign-alloc 'ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (%%iup-vbox-v array)
      (foreign-free array))))
