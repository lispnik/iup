(defpackage #:iup-im-cffi
  (:use #:common-lisp)
  (:import-from #:iup-cffi #:ihandle))

(in-package #:iup-im-cffi)

(cffi:define-foreign-library iup-im
  (:unix "libiupim.so")
  (:windows "iupim.dll")
  (t (:default "iupim")))

(cffi:use-foreign-library iup-im)

(cffi:defcfun (%iup-im-load-image "IupLoadImage") ihandle
  (filename :string))

(cffi:defcfun (%iup-im-save-image "IupSaveImage") :int
  (handle ihandle)
  (filename :string)
  (format :string))

(cffi:defcfun (%iup-im-load-animation "IupLoadAnimation") ihandle
  (filename :string))

(cffi:defcfun (%iup-im-load-animation-frames "IupLoadAnimation") ihandle
  (filename-list :string)
  (filename-count :int))

(cffi:defcfun (%iup-im-get-native-handle-image "IupGetNativeHandleImage")
    im-cffi::im-image
  (handle :pointer))

(cffi:defcfun (%iup-im-get-image-native-handle "IupGetImageNativeHandle")
    :pointer
  (handle im-cffi::im-image))

(cffi:defcfun (%iup-im-iup-image-from-im-image "IupImageFromImImage")
    ihandle
  (handle im-cffi::im-image))

(cffi:defcfun (%iup-im-iup-image-to-im-image "IupImageToImImage")
    im-cffi::im-image
  (handle ihandle))
