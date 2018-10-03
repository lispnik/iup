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

(defcommon-callback action-callback)
(defcommon-callback valuechanged-callback)
(defcommon-callback flataction-callback)
(defcommon-callback map-callback)
(defcommon-callback unmap-callback)
(defcommon-callback destroy-callback)
(defcommon-callback killfocus-callback)
(defcommon-callback enterwindow-callback)
(defcommon-callback leavewindow-callback)

(defmacro focus-callback ((handle focus) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int ((,handle iup-cffi::ihandle) (,focus :boolean))
      ,@body)))

(defmacro tabchange-callback ((handle new-tab-handle old-tab-handle) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,new-tab-handle iup-cffi::ihandle)
	 (,old-tab-handle iup-cffi::ihandle))
      ,@body)))


(defmacro tabchangepos-callback ((handle new-pos old-pos) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,new-pos :int)
	 (,old-pos :int))
      ,@body)))

(defmacro tabclosepos-callback ((handle pos) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,pos :int))
      ,@body)))

(defmacro rightclickpos-callback ((handle pos) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,pos :int))
      ,@body)))

(defmacro extrabutton-callback ((handle button pressed) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,button :int)
	 (,pressed :int))
      ,@body)))

;;; FIXME IupScrollbox has LAYOUTUPDATE_CB but function type not defined in docs
(defcommon-callback layoutupdate)

(defmacro detached-callback ((handle new-parent-handle x y) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,new-parent-handle iup-cffi::ihandle)
	 (,x :int)
	 (,y :int))
      ,@body)))

(defmacro restored-callback ((handle old-parent-handle x y) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,old-parent-handle iup-cffi::ihandle)
	 (,x :int)
	 (,y :int))
      ,@body)))

(defmacro openclose-callback ((handle state) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,state :int))
      ,@body)))

(defmacro button-callback ((handle button pressed x y status) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,button :int)
	 (,pressed :int)
	 (,x :int)
	 (,y :int)
	 (,status :string))
      ,@body)))


;; looks like these macros will need reimplementing in lisp for the status field above
;; iup_isshift(status)
;; iup_iscontrol(status)
;; iup_isbutton1(status)
;; iup_isbutton2(status)
;; iup_isbutton3(status)
;; iup_isbutton4(status)
;; iup_isbutton5(status)
;; iup_isdouble(status)
;; iup_isalt(status)
;; iup_issys(status)

(defmacro dropdown-callback ((handle state) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,state :int))
      ,@body)))

(defmacro dropshow-callback ((handle state) &body body)
  `(cffi:get-callback
    (cffi:defcallback ,(gensym) :int
	((,handle iup-cffi::ihandle)
	 (,state :int))
      ,@body)))


;;; last spot: IupCanvus








;;; k_any
;;; help
;;; action
