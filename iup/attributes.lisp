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

(defun attribute (handle attribute &optional (type 'string))
  (let ((value (iup-cffi::%iup-get-attribute handle attribute)))
    (ecase type
      (number (parse-number:parse-number value))
      (string value))))

(defun (setf attribute) (new-value handle attribute)
  (iup-cffi::%iup-set-str-attribute
   handle attribute (if new-value (princ-to-string new-value) (cffi:null-pointer)))
  new-value)

(defun attribute-id (handle attribute id &optional (type 'string))
  (let ((value (iup-cffi::%iup-get-attribute-id handle attribute id)))
    (ecase type
      (number (parse-number:parse-number value))
      (string value))))

(defun (setf attribute-id) (new-value handle attribute id)
  (iup-cffi::%iup-set-str-attribute-id
   handle attribute id (if new-value (princ-to-string new-value) (cffi:null-pointer)))
  new-value)

(defun attribute-id-2 (handle attribute line column &optional (type 'string))
  (let ((value (iup-cffi::%iup-get-attribute-id-2 handle attribute line column)))
    (ecase type
      (number (parse-number:parse-number value))
      (string value))))

(defun (setf attribute-id-2) (new-value handle attribute line column)
  (iup-cffi::%iup-set-str-attribute-id-2
   handle attribute line column (if new-value (princ-to-string new-value) (cffi:null-pointer)))
  new-value)

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
	  (cffi:foreign-free array))))))

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

(defalias save-class-attributes       #'iup-cffi::%iup-save-class-attributes)
(defalias copy-class-attributes       #'iup-cffi::%iup-copy-class-attributes)
(defalias set-class-default-attribute #'iup-cffi::%iup-set-class-default-attribute)

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

(defun (setf handle) (new-handle name)
  (iup-cffi::%iup-set-handle name new-handle)
  new-handle)

(defun handle (name)
  (iup-cffi::%iup-get-handle name))

