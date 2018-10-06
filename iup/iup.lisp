(in-package #:iup)

(defmacro alias (target source) `(setf (fdefinition ,target) ,source))

(defun handle-p (handle)
  (and (cffi:pointerp handle)
       (not (cffi:null-pointer-p handle))
       (= (char-code #\I) (cffi:mem-ref handle :char 0))
       (= (char-code #\U) (cffi:mem-ref handle :char 1))
       (= (char-code #\P) (cffi:mem-ref handle :char 2))
       (zerop (cffi:mem-ref handle :char 3))))

(defun get-classname-names (classname name-producer)
  (let* ((max-n (funcall name-producer classname (cffi:null-pointer) 0))
         (array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
    (unwind-protect
         (progn
           (funcall name-producer classname array max-n)
           (loop for i below max-n
                 for ref = (cffi:mem-aref array :pointer i)
                 until (cffi:null-pointer-p ref)
                 collect (make-keyword (cffi:foreign-string-to-lisp ref))))
      (foreign-free array))))

(defun class-attributes (classname)
  (get-classname-names classname #'iup-cffi::%iup-get-class-attributes))

(defun class-callbacks (classname)
  (get-classname-names classname #'iup-cffi::%iup-get-class-callbacks))

(defun all-classes ()
  (let* ((max-n (iup-cffi::%iup-get-all-classes (cffi:null-pointer) 0))
         (array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
    (unwind-protect
         (progn
           (iup-cffi::%iup-get-all-classes array max-n)
           (loop for i below max-n
                 for ref = (cffi:mem-aref array :pointer i)
                 until (cffi:null-pointer-p ref)
                 collect (cffi:foreign-string-to-lisp ref)))
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
  (iup-cffi::%iup-set-attribute-handle handle name new-value))

(defun callback (handle name)
  (iup-cffi::%iup-get-callback handle name))

(defun (setf callback) (new-value handle name)
  (iup-cffi::%iup-set-callback
   handle
   name
   (if new-value
       (cffi:get-callback new-value)
       (cffi:null-pointer))))

(defun attribute (handle attribute)
  (iup-cffi::%iup-get-attribute handle (symbol-name attribute)))

(defun (setf attribute) (new-value handle attribute)
  (iup-cffi::%iup-set-str-attribute handle attribute (or new-value (cffi:null-pointer))))

(defun (setf attributes) (attributes handle)
  (loop for (attribute value) on attributes by #'cddr
        do (progn
             (setf (attribute handle attribute) value))
        finally (return handle)))

(defun attributes (handle)
;;   (iup-cffi::%iup-get-all-attributes)
  ;; FIXME implement
  (error "not implemented")
  )

(defun (setf attribute-id) (new-value handle attribute id)
  ;; FIXME implement
  )

(defun (setf attribute-id-2) (new-value handle attribute line column)
  ;; FIXME implement
  )

(defun (setf attribute-callback-handle-dwim) (new-value handle name)
  (let ((name-string (symbol-name name)))
    (cond
      ((or (ends-with-subseq "_CB" name-string)
           (starts-with-subseq "K_" name-string))
       (setf (callback handle name) new-value))
      ((handle-p new-value)
       (setf (attribute-handle handle name) new-value))
      (t
       (setf (attribute handle name) new-value)))))

(defun (setf attribute-callback-handles-dwim) (names handle)
  (loop for (name value) on names by #'cddr
        do (progn
             (setf (attribute-callback-handle-dwim handle name) value))
        finally (return handle)))

(defun (setf handle) (new-value name)
  (iup-cffi::%iup-set-handle name new-value))

(defun handle (name)
  (iup-cffi::%iup-get-handle name))

(defmacro defattributefun (name args &rest body)
  `(defun ,name (,@args &rest attributes &key &allow-other-keys)
     (setf (attribute-callback-handles-dwim (progn ,@body)) attributes)))

;; (defmacro defattributefun (name args &rest body)
;;   `(defun ,name (,@args &rest attributes &key &allow-other-keys)
;;      (setf (attributes (progn ,@body)) attributes)))

(defconstant +error+      1)
(defconstant +noerror+    0)
(defconstant +opened+     -1)
(defconstant +invalid+    -1)
(defconstant +invalid-id+ -10)

(defconstant +ignore+   -1)
(defconstant +default+  -2)
(defconstant +close+    -3)
(defconstant +continue+ -4)

(defconstant +center+       #xffff)
(defconstant +left+         #xfffe)
(defconstant +right+        #xffd)
(defconstant +mousepos+     #xfffc)
(defconstant +current+      #xfffb)
(defconstant +centerparent+ #xfffa)
(defconstant +top+          +left+)
(defconstant +bottom+       +right+)

(defconstant +primary+   -1)
(defconstant +secondary+ -2)

(defconstant +recbinary+ 0)
(defconstant +rectext+   1)

(defun open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret +error+)
      (error "Can't initialize IUP"))))

(alias 'close #'iup-cffi::%iup-close)

(defmacro with-iup (() &body body)
  `(progn
     (iup:open)
     (unwind-protect
         (progn
           ,@body)
       (iup:close))))

(alias 'image-lib-open          #'iup-cffi::%iup-image-lib-open)
(alias 'main-loop               #'iup-cffi::%iup-main-loop)
(alias 'loop-step               #'iup-cffi::%iup-loop-step)
(alias 'loop-step-wait          #'iup-cffi::%iup-loop-step-wait)
(alias 'main-loop-level         #'iup-cffi::%iup-main-loop-level)
(alias 'flush                   #'iup-cffi::%iup-flush)
(alias 'exit-loop               #'iup-cffi::%iup-exit-loop)
(alias 'record-input            #'iup-cffi::%iup-record-input)
(alias 'play-input              #'iup-cffi::%iup-play-input)
(alias 'update                  #'iup-cffi::%iup-update)
(alias 'update-children         #'iup-cffi::%iup-update-children)
(alias 'redraw                  #'iup-cffi::%iup-redraw)
(alias 'refresh                 #'iup-cffi::%iup-refresh)
(alias 'refresh-children        #'iup-cffi::%iup-refresh-children)
(alias 'play-input              #'iup-cffi::%iup-play-input)
(alias 'version                 #'iup-cffi::%iup-version)
(alias 'version-date            #'iup-cffi::%iup-version-date)
(alias 'version-number          #'iup-cffi::%iup-version-number)
(alias 'destroy                 #'iup-cffi::%iup-destroy)
(alias 'detach                  #'iup-cffi::%iup-detach)
(alias 'append                  #'iup-cffi::%iup-append)
(alias 'insert                  #'iup-cffi::%iup-insert)
(alias 'get-child               #'iup-cffi::%iup-get-child)
(alias 'get-child-pos           #'iup-cffi::%iup-get-child-pos)
(alias 'get-child-count         #'iup-cffi::%iup-get-child-count)
(alias 'get-next-child          #'iup-cffi::%iup-get-next-child)
(alias 'get-brother             #'iup-cffi::%iup-get-brother)
(alias 'get-parent              #'iup-cffi::%iup-get-parent)
(alias 'get-dialog              #'iup-cffi::%iup-get-dialog)
(alias 'get-dialog-child        #'iup-cffi::%iup-get-dialog-child)
(alias 'reparent                #'iup-cffi::%iup-reparent)
(alias 'popup                   #'iup-cffi::%iup-popup)
(alias 'show                    #'iup-cffi::%iup-show)
(alias 'show-xy                 #'iup-cffi::%iup-show-xy)
(alias 'hide                    #'iup-cffi::%iup-hide)
(alias 'map                     #'iup-cffi::%iup-map)
(alias 'unmap                   #'iup-cffi::%iup-unmap)
(alias 'reset-attribute         #'iup-cffi::%iup-reset-attribute)
(alias 'set-gloabl              #'iup-cffi::%iup-set-str-global)
(alias 'get-gloabl              #'iup-cffi::%iup-get-global)
(alias 'set-focus               #'iup-cffi::%iup-set-focus)
(alias 'get-focus               #'iup-cffi::%iup-get-focus)
(alias 'previous-field          #'iup-cffi::%iup-previous-field)
(alias 'next-field              #'iup-cffi::%iup-next-field)

(defattributefun fill () (iup-cffi::%iup-fill))
(defattributefun space () (iup-cffi::%iup-space))
(defattributefun radio (child) (iup-cffi::%iup-radio child))

(defmacro defattributefun-children (name func)
  `(defattributefun ,name (children)
     (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
       (unwind-protect
            (,func array)
         (foreign-free array)))))

(defattributefun-children vbox          iup-cffi::%iup-vbox-v)
(defattributefun-children zbox          iup-cffi::%iup-zbox-v)
(defattributefun-children hbox          iup-cffi::%iup-hbox-v)
(defattributefun-children normalizer    iup-cffi::%iup-normalizer-v)
(defattributefun-children cbox          iup-cffi::%iup-cbox-v)

(defattributefun sbox                   (child) (iup-cffi::%iup-sbox child))
(defattributefun split                  (child1 child2) (iup-cffi::%iup-split child1 child2))
(defattributefun scroll-box             (child) (iup-cffi::%iup-scroll-box child))
(defattributefun flat-scroll-box        (child) (iup-cffi::%iup-flat-scroll-box child))

(defattributefun-children grid-box      iup-cffi::%iup-gridbox-v)

(defattributefun expander       (child) (iup-cffi::%iup-expander child))
(defattributefun detach-box     (child) (iup-cffi::%iup-detach-box child))
(defattributefun background-box (child) (iup-cffi::%iup-background-box child))
(defattributefun frame          (child) (iup-cffi::%iup-frame child))
(defattributefun flat-frame     (child) (iup-cffi::%iup-flat-frame child))

(defattributefun image (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height))))
    (unwind-protect
	 (iup-cffi::%iup-image width height array)
      (cffi:foreign-free array))))

(defattributefun image-rgb (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height 3))))
    (unwind-protect
	 (iup-cffi::%iup-image-rgb width height array)
      (cffi:foreign-free array))))

(defattributefun image-rgba (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height 4))))
    (unwind-protect
	 (iup-cffi::%iup-image-rgba width height array)
      (cffi:foreign-free array))))

(defattributefun item () (iup-cffi::%iup-item (cffi:null-pointer) (cffi:null-pointer)))

(alias 'separator #'iup-cffi::%iup-separator)

(defattributefun submenu (menu) (iup-cffi::%iup-submenu (cffi:null-pointer) menu))

(defattributefun-children menu iup-cffi::%iup-menu-v)

(defattributefun button         () (iup-cffi::%iup-button (cffi:null-pointer) (cffi:null-pointer)))
(defattributefun flat-button    () (iup-cffi::%iup-flat-button (cffi:null-pointer)))
(defattributefun flat-toggle    () (iup-cffi::%iup-flat-toggle (cffi:null-pointer)))
(defattributefun drop-button    () (iup-cffi::%iup-drop-button (cffi:null-pointer)))
(defattributefun flat-label     () (iup-cffi::%iup-flat-label (cffi:null-pointer)))
(defattributefun flat-separator () (iup-cffi::%iup-flat-separator))
(defattributefun canvas         () (iup-cffi::%iup-canvas (cffi:null-pointer)))
(defattributefun dialog         (child) (iup-cffi::%iup-dialog child))
(defattributefun user           () (iup-cffi::%iup-user))
(defattributefun label          () (iup-cffi::%iup-label (cffi:null-pointer)))
(defattributefun list           () (iup-cffi::%iup-list (cffi:null-pointer)))
(defattributefun text           () (iup-cffi::%iup-text (cffi:null-pointer)))
(defattributefun multi-line     () (iup-cffi::%iup-multi-line (cffi:null-pointer)))
(defattributefun toggle         () (iup-cffi::%iup-toggle (cffi:null-pointer) (cffi:null-pointer)))
(defattributefun timer          () (iup-cffi::%iup-timer))
(defattributefun clipboard      () (iup-cffi::%iup-clipboard))
(defattributefun progress-bar   () (iup-cffi::%iup-progress-bar))
(defattributefun val            () (iup-cffi::%iup-val (cffi:null-pointer)))

(defattributefun-children tabs      iup-cffi::%iup-tabs-v)
(defattributefun-children flat-tabs iup-cffi::%iup-flat-tabs-v)

(defattributefun tree            () (iup-cffi::%iup-tree))
(defattributefun link            () (iup-cffi::%iup-link (cffi:null-pointer) (cffi:null-pointer)))
(defattributefun animated-label  () (iup-cffi::%iup-animated-label (cffi:null-pointer)))
(defattributefun date-pick       () (iup-cffi::%iup-date-pick))
(defattributefun calendar        () (iup-cffi::%iup-calendar))
(defattributefun colorbar        () (iup-cffi::%iup-colorbar))
(defattributefun gauge           () (iup-cffi::%iup-gauge))
(defattributefun dial            () (iup-cffi::%iup-dial (cffi:null-pointer)))
(defattributefun color-browser   () (iup-cffi::%iup-color-browser))
(defattributefun file-dialog     () (iup-cffi::%iup-file-dlg))
(defattributefun message-dialog  () (iup-cffi::%iup-message-dlg))
(defattributefun color-dialog    () (iup-cffi::%iup-color-dlg))
(defattributefun font-dialog     () (iup-cffi::%iup-font-dlg))
(defattributefun progress-dialog () (iup-cffi::%iup-progress-dlg))

(alias 'message       #'iup-cffi::%iup-message)
(alias 'message-error #'iup-cffi::%iup-message-error)
(alias 'message-alarm #'iup-cffi::%iup-message-alarm)
(alias 'alarm         #'iup-cffi::%iup-alarm)

(defattributefun config () (iup-cffi::%iup-config))

(alias 'config-load          #'iup-cffi::%iup-config-load)
(alias 'config-save          #'iup-cffi::%iup-config-save)
(alias 'config-dialog-show   #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)
