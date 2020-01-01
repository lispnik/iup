(defpackage #:iup-classesdb-cffi (:use #:common-lisp))
(in-package #:iup-classesdb-cffi)

(cffi:defcenum native-type
  :iup-type-void
  :iup-type-control
  :iup-type-canvas
  :iup-type-dialog
  :iup-type-image
  :iup-type-menu
  :iup-type-other)

(cffi:defcstruct iclass
  (name :string)
  (cons :string)
  (format :string)
  (format-attr :string)
  (native-type native-type)
  (child-type :int) 			;0 = none, 1 = many, 1+n = n children
  (interactive-p :boolean)
  (has-attrib-id-p :int)
  (internal-p :boolean)
  (parent :pointer)
  (attrib-func :pointer))

(cffi:defbitfield attrib-flags
  (:iupaf-default 0)
  :iupaf-no-inherit
  :iupaf-no-defaultvalue
  :iupaf-no-string
  :iupaf-not-mapped
  :iupaf-has-id
  :iupaf-readonly
  :iupaf-writeonly
  :iupaf-has-id2
  :iupaf-callback
  :iupaf-no-save
  :iupaf-not-supported
  :iupaf-ihandlename
  :iupaf-ihandle)

(cffi:defcfun (%iup-register-find-class "iupRegisterFindClass") :pointer
  (class :string))

(cffi:defcfun (%iup-class-register-get-attribute "iupClassRegisterGetAttribute") :void
  (class :pointer)
  (name :string)
  (get-func :pointer)
  (set-func :pointer)
  (default-value :pointer)
  (system-default :pointer)
  (flag :pointer))

(cffi:defcfun (%iup-table-get-curr-type "iupTableGetCurrType") :int
  (table :pointer))

(cffi:defcfun (%iup-table-first "iupTableFirst") :string
  (table :pointer))

(cffi:defcfun (%iup-table-next "iupTableNext") :string
  (table :pointer))


