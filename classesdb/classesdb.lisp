(defpackage #:iup-classesdb
  (:use #:common-lisp
        #:alexandria)
  (:export #:regenerate))

(in-package #:iup-classesdb)

(defun attribute-metadata (class attrib table)
  (cffi:with-foreign-objects
      ((get-func :pointer)
       (set-func :pointer)
       (default-value :pointer)
       (system-default :pointer)
       (flags :int))
    (iup-classesdb-cffi::%iup-class-register-get-attribute
     class
     attrib
     get-func
     set-func
     default-value
     system-default
     flags)
    (list
     :type (iup-classesdb-cffi::%iup-table-get-curr-type table)
     :default-value (cffi:foreign-string-to-lisp (cffi:mem-aref default-value :pointer))
     :system-default (cffi:foreign-string-to-lisp (cffi:mem-aref system-default :pointer))
     :flags (cffi:foreign-bitfield-symbols 'iup-classesdb-cffi::attrib-flags (cffi:mem-ref flags :int)))))

(defun attribute-table (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::attrib-func) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::attrib-func))

(defun class-metadata (classname)
  (let* ((class (iup-classesdb-cffi::%iup-register-find-class classname))
         (table (attribute-table class)))
    (loop for attrib = (iup-classesdb-cffi::%iup-table-first table)
            then (iup-classesdb-cffi::%iup-table-next table)
          while attrib
          collect (list* :name attrib (attribute-metadata class attrib table)))))

(defun all-classes ()
  (flet ((iup-get-all-classes (names n)
           (iup-cffi::%iup-get-all-classes names n)))
    (let* ((max-n (iup-get-all-classes (cffi:null-pointer) 0))
           (array (cffi:foreign-alloc :pointer
                                      :initial-element (cffi:null-pointer)
                                      :count max-n
                                      :null-terminated-p t)))
      (unwind-protect
           (progn
             (iup-get-all-classes array max-n)
             (loop for i below max-n
                   for ref = (cffi:mem-aref array :pointer i)
                   until (cffi:null-pointer-p ref)
                   collect (cffi:foreign-string-to-lisp ref) into result
                   finally (return (sort result #'string<))))
        (cffi:foreign-free array)))))

(defmacro with-iup (&body body)
  (with-gensyms (result)
    `(unwind-protect
          (let ((,result (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
            (unless (zerop ,result)
              (error "Can't load IUP"))
            ,@body)
       (iup-cffi::%iup-close))))

(defun classes-metadata ()
  (loop for classname in (all-classes)
        collect
        (list :classname
              classname
              :attributes             
              (class-metadata classname))))

(defparameter *static-metadata*
  ;; :library names the shared object per platform and :init-function the C
  ;; entry point, as data rather than as symbols in the addon -cffi packages.
  ;; Symbols would make merely READING this list require every addon system,
  ;; and their libraries, to be present -- and most of the addons have no
  ;; library at all in lispnik/tecgraf-iup yet. As data, an entry whose
  ;; library will not load is SKIPPED, its existing metadata carried over,
  ;; and regeneration refreshes whatever the running platform can actually
  ;; introspect.
  '((:init-function "IupOpen"
     :package "IUP"
     :override-p ("image" "imagergb" "imagergba")
     :vanity-alist (("gridbox" . "grid-box")
                    ("flatframe" . "flat-frame")
                    ("flattabs" . "flat-tabs")
                    ("flatlist" . "flat-list")
                    ("backgroundbox" . "background-box")
                    ("scrollbox" . "scroll-box")
                    ("flatscrollbox" . "flat-scroll-box")
                    ("detachbox" . "detach-box")
                    ("animatedlabel" . "animated-label")
                    ("flatbutton" . "flat-button")
                    ("dropbutton" . "drop-button")
                    ("colorbrowser" . "color-browser")
                    ("datepick" . "date-pick")
                    ("flatlabel" . "flat-label")
                    ("flatseparator" . "flat-separator")
                    ("progressbar" . "progress-bar")
                    ("flattoggle" . "flat-toggle")
                    ("multiline" . "multi-line")
                    ("submenu" . "sub-menu")
                    ("colordlg" . "color-dialog")
                    ("filedlg" . "file-dialog")
                    ("fontdlg" . "font-dialog")
                    ("messagedlg" . "message-dialog")
                    ("progressdlg" . "progress-dialog")
                    ("parambox" . "param-box")
                    ("imagergb" . "image-rgb")
                    ("imagergba" . "image-rgba")
                    ("multibox" . "multi-box")))
    (:library ((:darwin (:framework "iupcontrols")) (:unix "libiupcontrols.so") (:windows "iupcontrols.dll"))
     :init-function "IupControlsOpen"
     :package "IUP-CONTROLS"
     :vanity-alist (("matrixex" . "matrix-ex")
                    ("matrixlist" . "matrix-list")))
    (:library ((:darwin (:framework "iupgl")) (:unix "libiupgl.so") (:windows "iupgl.dll"))
     :init-function "IupGLCanvasOpen"
     :package "IUP-GL"
     :vanity-alist (("glcanvas" . "canvas")
                    ("glsubcanvas" . "sub-canvas")))
    (:library ((:darwin (:framework "iupglcontrols")) (:unix "libiupglcontrols.so") (:windows "iupglcontrols.dll"))
     :init-function "IupGLControlsOpen"
     :package "IUP-GLCONTROLS"
     :vanity-alist (("glcanvasbox" . "canvas-box")
                    ("glsubcanvas" . "sub-canvas")
                    ("glprogressbar" . "progress-bar")
                    ("glscrollbox" . "scroll-box")
                    ("glsizebox" . "size-box")
                    ("glval" . "val")
                    ("gltoggle" . "toggle")     
                    ("gltext" . "text")
                    ("gllink" . "link") 
                    ("glframe" . "frame")
                    ("glexpander" . "expander") 
                    ("glbutton" . "button")     
                    ("gllabel" . "label")
                    ("glseparator" . "separator")
                    ("glbackgroundbox" . "background-box")))
    (:library ((:darwin (:framework "iup_plot")) (:unix "libiup_plot.so") (:windows "iup_plot.dll"))
     :init-function "IupPlotOpen"
     :package "IUP-PLOT")
    (:library ((:darwin (:framework "iup_mglplot")) (:unix "libiup_mglplot.so") (:windows "iup_mglplot.dll"))
     :init-function "IupMglPlotOpen"
     :package "IUP-MGLPLOT"
     :vanity-alist (("mglplot" . "plot")
                    ("mgllabel" . "label")))
    #+windows
    (:library ((:windows "iupole.dll"))
     :init-function "IupOleControlOpen"
     :package "IUP-OLECONTROL")
    (:library ((:darwin (:framework "iup_scintilla")) (:unix "libiup_scintilla.so") (:windows "iup_scintilla.dll"))
     :init-function "IupScintillaOpen"
     :package "IUP-SCINTILLA"
     :vanity-alist (("scintilladlg" . "scintilla-dialog")))
    (:library ((:darwin (:framework "iupweb")) (:unix "libiupweb.so") (:windows "iupweb.dll"))
     :init-function "IupWebBrowserOpen"
     :package "IUP-WEB"
     :classname-excludes ("olecontrol")
     :vanity-alist (("webbrowser" . "web-browser")))
    (:library ((:darwin (:framework "iuptuio")) (:unix "libiuptuio.so") (:windows "iuptuio.dll"))
     :init-function "IupTuioOpen"
     :package "IUP-TUIO"
     :override-p ("tuioclient")
     :vanity-alist (("tuioclient" . "client"))))
  "Information on how to create the Lisp bindings.

:INITIALIZER function to call which initializes a specific IUP library
:PACKAGE the name of a package from which the Lisp bindings should be export
:CLASSNAME-EXCLUDES a list of IUP class names to exclude

:OVERRIDE-P list of IUP class names which should not be created
automatically (e.g. because they require a specific argument lists at
creation)

:VANITY-ALIST a mapping between IUP names and Lisp names")

(defparameter *platform* 
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)

(defun class-format (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::format) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::format))

(defun class-child-type (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::child-type) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::child-type))

(defun child-spec-from-format (format)
  "Returns :CHILD-NONE, :CHILD-MANY or an integer count of children."
  (cond ((find #\g format)
         :child-many)
        ((find #\h format)
         (count #\h format))
        (t :child-none)))

(defun %entry-available-p (metadata)
  "Load the entry's library and return its initializer as a callable, or NIL.

NIL means the running IUP simply does not provide this addon -- most of them
have no CMake target in lispnik/tecgraf-iup yet -- and the entry's existing
metadata is carried over unchanged rather than dropped."
  (let ((library (getf metadata :library))
        (init-name (getf metadata :init-function)))
    (when library
      ;; The clauses are keyed like DEFINE-FOREIGN-LIBRARY's: pick the first
      ;; whose feature is present. :darwin is listed before :unix for the
      ;; same reason it is in every -cffi file -- darwin satisfies both.
      (let ((clause (find-if #'(lambda (clause)
                                 (member (first clause) *features*))
                             library)))
        (unless (and clause
                     (ignore-errors (cffi:load-foreign-library (second clause))))
          (return-from %entry-available-p nil))))
    (if (string= init-name "IupOpen")
        ;; The core: WITH-IUP has already opened it.
        (constantly nil)
        (let ((pointer (cffi:foreign-symbol-pointer init-name)))
          (when pointer
            (lambda ()
              ;; :void regardless of the true return type: some initializers
              ;; return int, and discarding a register is safe where reading
              ;; one from a void function is not.
              (cffi:foreign-funcall-pointer pointer () :void)))))))

(defun create-classesdb ()
  "Create a printable representaion of IUP metadata containing enough
information to create the Lisp API at compilation time.

Returns (VALUES platform-section regenerated-package-names): only packages
whose addon library loaded are in the section, and the caller merges the
rest forward from the previous database."
  (flet ((vanity-name (vanity-alist classname)
           (if-let (vanity-name (assoc-value vanity-alist classname :test #'string=))
             (string-upcase vanity-name))))
    (loop with base-classnames = (with-iup (all-classes))
          for metadata in *static-metadata*
          for package = (getf metadata :package)
          for core-p = (string= (getf metadata :init-function) "IupOpen")
          for initializer = (%entry-available-p metadata)
          unless initializer
            do (format t "~&Skipping ~A: its library did not load here" package)
          when initializer
            collect package into regenerated
          when initializer
            collect
            (let* ((classes (with-iup (funcall initializer) (all-classes)))
                   (classname-excludes (getf metadata :classname-excludes))
                   (difference (remove-if #'(lambda (classname)
                                              (find classname classname-excludes :test #'string=))
                                          (if core-p
                                              base-classnames
                                              (set-difference classes base-classnames :test #'string=))))
                   (override-p (getf metadata :override-p))
                   (vanity-alist (getf metadata :vanity-alist)))
              (format t "~&Processing for package ~A" package)
              (with-iup
                (funcall initializer)
                (list :package package
                      :classnames
                      (loop for classname in difference
                            for class = (iup-classesdb-cffi::%iup-register-find-class classname)
                            for class-format = (class-format class)
                            collect
                            (list :classname classname
                                  :format class-format
                                  :children (child-spec-from-format class-format)
                                  :override-p (and (find classname override-p :test #'string=) t)
                                  :vanity-classname (vanity-name vanity-alist classname)
                                  :attributes (class-metadata classname))))))
            into result
          finally (return (values (append (list :platform *platform*)
                                          (list :metadata result))
                                  regenerated)))))

(defun classesdb-pathname ()
  (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))

(defun read-classesdbs ()
  (let ((classesdb-pathname (classesdb-pathname)))
    (if (probe-file classesdb-pathname)
        (with-open-file (stream classesdb-pathname :direction :input)
          (let ((*read-eval* nil))
            (read stream stream)))
        '((:platform :linux)
          (:platform :windows)
          (:platform :unix)))))

(defun update-classesdbs (current-classesdbs classesdb regenerated)
  "Merge at the PACKAGE level, not the platform level.

Replacing a platform's whole section would silently drop the metadata for
every package whose addon library is not built yet -- IUP-PLOT, IUP-SCINTILLA
and friends -- when the point of keeping a database at all is that the API
survives what the current machine cannot introspect. Packages actually
regenerated are replaced; the rest ride along from the previous file."
  (let ((our-platform *platform*))
    (mapcar #'(lambda (existing-classesdb)
                (if (eq (getf existing-classesdb :platform) our-platform)
                    (let ((old-metadata (getf existing-classesdb :metadata))
                          (new-metadata (getf classesdb :metadata)))
                      (list :platform our-platform
                            :metadata
                            (append new-metadata
                                    (remove-if #'(lambda (package)
                                                   (find (getf package :package) regenerated
                                                         :test #'string=))
                                               old-metadata))))
                    existing-classesdb))
            current-classesdbs)))

(defun write-classesdbs (classesdbs)
  (with-open-file (stream (classesdb-pathname) :direction :output :if-exists :supersede)
    (format stream ";;; generated at ~A for IUP ~A -*-lisp-*-~%~%"
            (local-time:format-timestring nil (local-time:universal-to-timestamp (get-universal-time))
                                          :timezone local-time:+utc-zone+)
            (with-iup (iup-cffi::%iup-version)))
    (write classesdbs :stream stream :pretty t :right-margin 100)))

(defun regenerate ()
  (multiple-value-bind (new-classesdb regenerated) (create-classesdb)
    (write-classesdbs
     (update-classesdbs (read-classesdbs) new-classesdb regenerated))
    (format t "~&Regenerated on ~A: ~{~A~^, ~}~%" *platform* regenerated)
    nil))
