(defpackage #:iup-cffi
  (:use #:common-lisp
	#:cffi
	#:alexandria))

(in-package #:iup-cffi)

(define-foreign-library iup
  (:unix "libiup.so")
  (t (:default "iup")))

(use-foreign-library iup)

(defconstant %iup-error 1)
(defconstant %iup-noerror 0)
(defconstant %iup-opened -1)
(defconstant %iup-invalid -1)
(defconstant %iup-invalid-id -10)

(defcfun (%iup-open "IupOpen") :int
  (argv :pointer)
  (argc :pointer))

(defcfun (%iup-close "IupClose") :void)

(defcfun (%iup-message "IupMessage") :void
  (title :string)
  (message :string))

(defcfun (%iup-version "IupVersion") :string)

(defcfun (%iup-version-number "IupVersionNumber") :int)

(defctype ihandle :pointer)

(defcfun (%iup-label "IupLabel") ihandle
  (title :string))

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

(defcfun (%iup-vbox-v "IupVBoxv") ihandle
  (children :pointer))

(defcfun (%iup-button "IupButton") ihandle
  (title :string)
  (action :pointer))

(defcfun (%iup-main-loop "IupMainLoop") :int)
