(in-package #:iup)

(alias 'config #'iup-cffi::%iup-config)
(alias 'config-load          #'iup-cffi::%iup-config-load)
(alias 'config-save          #'iup-cffi::%iup-config-save)
(alias 'config-dialog-show   #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)

;;; FIXME other config functions

(defun config-value (handle group key config-value-type &optional default)
  (ecase config-value-type
    (string
     (if default
	 (iup-cffi::%iup-config-get-variable-str-def handle group key (princ default))
	 (iup-cffi::%iup-config-get-variable-str handle group key)))
    (integer
     (if default
	 (iup-cffi::%iup-config-get-variable-int-def handle group key default)
	 (iup-cffi::%iup-config-get-variable-int handle group key))) 
    (double-float
     (if default
	 (iup-cffi::%iup-config-get-variable-double-def handle group key (coerce default 'double-float))
	 (iup-cffi::%iup-config-get-variable-double handle group key)))))

(defun (setf config-value) (new-value handle group key config-value-type)
  ;; FIXME
  )

(defun config-value-id (handle group key id config-value-type &optional default)
  (ecase config-value-type
    (string
     (if default
	 (iup-cffi::%iup-config-get-variable-str-id-def handle group key id (princ default))
	 (iup-cffi::%iup-config-get-variable-str-id handle group key id)))
    (integer
     (if default
	 (iup-cffi::%iup-config-get-variable-int-id-def handle group key id default)
	 (iup-cffi::%iup-config-get-variable-int-id handle group key id)))
    (double-float
     (if default
	 (iup-cffi::%iup-config-get-variable-double-id-def handle group key id (coerce default 'double-float))
	 (iup-cffi::%iup-config-get-variable-double-id handle group key id)))))

(defun (setf config-value-id) (new-value handle group key id config-value-type)
  ;; FIXME
  )
