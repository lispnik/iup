(in-package #:iup)

(defmacro idle-action (() &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int ()
      ,@body)))

(defmacro globalctrlfunc-callback ((c) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int ((,c :int))
      ,@body)))

(defmacro defcommon-callback (name)
  `(defmacro ,name ((handle) &body body)
    `(cffi:get-callback
      (cffi:defcallback ,(gensym) :int ((,handle iup-cffi::ihandle))
	,@body))))

(defcommon-callback map-callback)
(defcommon-callback unmap-callback)
(defcommon-callback destroy-callback)
(defcommon-callback killfocus-callback)
(defcommon-callback enterwindow-callback)
(defcommon-callback leavewindow-callback)

;;; k_any
;;; help
;;; action
