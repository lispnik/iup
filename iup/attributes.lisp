(in-package #:iup)

(export '(class-attributes
	  class-callbacks
	  all-classes
	  class-name
	  class-type
	  save-class-attributes
	  copy-class-attributes
	  ;; set-class-default-attribute
	  attribute-handle
	  callback
	  handle
	  attribute))

(deftype attribute-type ()
  '(member integer single-float double-float string cffi:foreign-pointer))

(defun attribute (handle attribute &optional (type 'string))
  (declare (type attribute-type type))
  (ecase type
    (integer (iup-cffi::%iup-get-int-attribute handle attribute))
    (single-float (iup-cffi::%iup-get-float-attribute handle attribute))
    (double-float (iup-cffi::%iup-get-double-attribute handle attribute))
    (string (iup-cffi::%iup-get-attribute handle attribute))
    (cffi:foreign-pointer (iup-cffi::%iup-get-pointer-attribute handle attribute))))

(defun (setf attribute) (new-value handle attribute)
  (typecase new-value
    (string (iup-cffi::%iup-set-str-attribute handle attribute (or new-value (cffi:null-pointer))))
    (integer (iup-cffi::%iup-set-int-attribute handle attribute new-value))
    (single-float (iup-cffi::%iup-set-float-attribute handle attribute new-value))
    (double-float (iup-cffi::%iup-set-double-attribute handle attribute new-value))
    (cffi:foreign-pointer (iup-cffi::%iup-set-attribute handle attribute new-value))
    (t (iup-cffi::%iup-set-str-attribute
	handle
	attribute
	(if new-value
	    (write-to-string new-value)
	    (cffi:null-pointer)))))
  new-value)


;; (defun (setf attributes) (attributes handle)
;;   (loop for (attribute value) on attributes by #'cddr
;;         do (progn
;;              (setf (attribute handle attribute) value))
;;         finally (return attributes)))

;; (defun attributes (handle)
;; ;;   (iup-cffi::%iup-get-all-attributes)
;;   ;; FIXME implement
;;   (error "not implemented")
;;   )

;; (defun (setf attribute-id) (new-value handle attribute id)
;;   ;; FIXME implement
;;   )

;; (defun attribute-id-2 (handle attribute line column &optional (type :string))
;;   (declare (type attribute-type type))
;;   (case type
;;     (:int (iup-cffi::%iup-get-int-attribute-id-2 handle attribute line column))
;;     (:float (iup-cffi::%iup-get-float-attribute-id-2 handle attribute line column))
;;     (:double (iup-cffi::%iup-get-double-attribute-id-2 handle attribute line column))
;;     (:string (iup-cffi::%iup-get-attribute-id-2 handle attribute line column))))

;; (defun (setf attribute-id-2) (new-value handle attribute line column)
;;   (typecase new-value
;;     (string (iup-cffi::%iup-set-str-attribute-id-2 handle attribute line column (or new-value (cffi:null-pointer))))
;;     (integer (iup-cffi::%iup-set-int-attribute-id-2 handle attribute line column new-value))
;;     (single-float (iup-cffi::%iup-set-float-attribute-id-2 handle attribute line column new-value))
;;     (double-float (iup-cffi::%iup-set-double-attribute-id-2 handle attribute line column new-value))
;;     (t (iup-cffi::%iup-set-str-attribute-id-2
;; 	handle
;; 	attribute
;; 	line
;; 	column
;; 	(if new-value
;; 	    (princ new-value)
;; 	    (cffi:null-pointer))))))

;; (defun (setf attribute-callback-handle-dwim) (new-value handle name)
;;   (let ((name-string (symbol-name name)))
;;     (cond
;;       ((or (string= "ACTION" name-string)
;; 	   (ends-with-subseq "_CB" name-string)
;;            (starts-with-subseq "K_" name-string))
;;        (setf (callback handle name) new-value))
;;       ((handle-p new-value)
;;        (setf (attribute-handle handle name) new-value))
;;       (t
;;        (setf (attribute handle name) new-value)))))

;; (defun (setf attribute-callback-handles-dwim) (names handle)
;;   (loop for (name value) on names by #'cddr
;;         do (progn
;;              (setf (attribute-callback-handle-dwim handle name) value))
;;         finally (return handle)))

(defun classname-names (classname name-producer)
  (let ((max-n (funcall name-producer classname (cffi:null-pointer) 0)))
    (unless (= max-n -1)
      (let ((array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
	(unwind-protect
	     (progn
	       (funcall name-producer classname array max-n)
	       (loop for i below max-n
		     for ref = (cffi:mem-aref array :pointer i)
		     until (cffi:null-pointer-p ref)
		     collect (make-keyword (cffi:foreign-string-to-lisp ref))))
	  (foreign-free array))))))

(defun class-attributes (classname)
  (classname-names classname #'iup-cffi::%iup-get-class-attributes))

(defun class-callbacks (classname)
  (classname-names classname #'iup-cffi::%iup-get-class-callbacks))

(defun all-classes ()
  (let* ((max-n (iup-cffi::%iup-get-all-classes (cffi:null-pointer) 0))
         (array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
    (unwind-protect
         (progn
           (iup-cffi::%iup-get-all-classes array max-n)
           (loop for i below max-n
                 for ref = (cffi:mem-aref array :pointer i)
                 until (cffi:null-pointer-p ref)
                 collect (cffi:foreign-string-to-lisp ref) into result
		 finally (return (sort result #'string<))))
      (cffi:foreign-free array))))

(defun class-name (handle)
  (iup-cffi::%iup-get-class-name handle))

(defun class-type (handle)
  (iup-cffi::%iup-get-class-type handle))

(alias 'save-class-attributes       #'iup-cffi::%iup-save-class-attributes)
(alias 'copy-class-attributes       #'iup-cffi::%iup-copy-class-attributes)
(alias 'set-class-default-attribute #'iup-cffi::%iup-set-class-default-attribute)

(defun attribute-handle (handle name)
  (iup-cffi::%iup-get-attribute-handle handle name))

(defun (setf attribute-handle) (new-value handle name)
  (iup-cffi::%iup-set-attribute-handle handle name new-value)
  new-value)

(defun callback (handle name)
  (find-callback name handle))

(defun (setf callback) (new-value handle name)
  (if new-value
      (progn
	(let* ((callback (class-callback-name (class-name handle) name (find-package "IUP")))
	       (cffi-callback (cffi:get-callback callback))
	       (callback-name (symbol-name name)))
	  (register-callback callback handle new-value)
	  (iup-cffi::%iup-set-callback handle callback-name cffi-callback)))
      (unregister-callback name handle))
  new-value)

(defun (setf handle) (new-value name)
  (iup-cffi::%iup-set-handle name new-value))

(defun handle (name)
  (iup-cffi::%iup-get-handle name))

