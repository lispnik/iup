(defpackage #:iup-im-cffi
  (:use #:common-lisp))

(in-package #:iup-im-cffi)

(cffi:define-foreign-library iup-im
  (:unix "libim.so")
  (:windows "iupim.dll")
  (t (:default "iupim")))

(cffi:use-foreign-library iup-im)

(cffi:defcfun (%iup-im-load-image "IupLoadImage") iup-cffi::ihandle
  (filename :string))

(cffi:defcfun (%iup-im-save-image "IupSaveImage") :int
  (handle iup-cffi::ihandle)
  (filename :string)
  (format :string))

(cffi:defcfun (%iup-im-load-animation "IupLoadAnimation") iup-cffi::ihandle
  (filename :string))

(cffi:defcfun (%iup-im-load-animation-frames "IupLoadAnimation") iup-cffi::ihandle
  (filename-list :string)
  (filename-count :int))

(cffi:defcfun (%iup-im-get-native-handle-image "IupGetNativeHandleImage")
    im-cffi::im-image
  (handle :pointer))

(cffi:defcfun (%iup-im-get-image-native-handle "IupGetImageNativeHandle")
    :pointer
  (handle im-cffi::im-image))

(cffi:defcfun (%iup-im-iup-image-from-im-image "IupImageFromImImage")
    iup-cffi::ihandle
  (handle im-cffi::im-image))

(cffi:defcfun (%iup-im-iup-image-to-im-image "IupImageToImImage")
    im-cffi::im-image
  (handle iup-cffi::ihandle))
