(defpackage #:iup-classesdb
  (:use #:common-lisp
        #:alexandria)
  (:export #:regenerate-from-dumps))

(in-package #:iup-classesdb)

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
    ;; Not #+windows: this is policy consumed by a transformer that may run
    ;; anywhere. Whether OLE classes exist is decided by which dump mentions
    ;; IupOleControlOpen, not by the machine doing the transforming.
    (:library ((:windows "iupole.dll"))
     :init-function "IupOleControlOpen"
     ;; Claimed, not discovered: on Windows the web browser is OLE-based, so
     ;; IupWebBrowserOpen registers olecontrol first and sequential
     ;; attribution hands it to IUP-WEB -- whose excludes then drop it
     ;; entirely. A claim pulls the class here from wherever the dump
     ;; recorded it, provided this module ran at all.
     :classname-claims ("olecontrol")
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

;;; ---------------------------------------------------------------------------
;;; From here down this file consumes the classes.sexp files that
;;; lispnik/tecgraf-iup now produces and ships in its release archives
;;; (share/iup/classes.sexp), written by tools/iup_classdump.c there.
;;;
;;; It used to introspect a running IUP over FFI against internal SDK
;;; symbols, which needed a GUI session on three platforms, a chained CI
;;; workflow to merge them, and internal symbols that were only exported by
;;; accident -- and its output still went six years stale, because nothing
;;; tied the snapshot to a build. Metadata produced by and shipped with the
;;; exact build it describes cannot drift from it, and turning it into the
;;; database is now pure list manipulation: no FFI, no display, one process.
;;;
;;; The dump is facts; this file is policy. Which classes belong to which
;;; package, their Lisp vanity names and which constructors are hand-written
;;; all come from *static-metadata* above, joined to the dump's modules by
;;; the C initializer name.

(defun child-spec-from-format (format)
  "Returns :CHILD-NONE, :CHILD-MANY or an integer count of children."
  (cond ((find #\g format) :child-many)
        ((find #\h format) (count #\h format))
        (t :child-none)))

(defun read-dump (pathname)
  "Read one classes.sexp as (VALUES plist iup-version-string)."
  (with-open-file (stream pathname)
    (let* ((header (read-line stream))
           (version (let ((mark (search "IUP " header)))
                      (if mark
                          (remove-if-not #'(lambda (c) (or (digit-char-p c) (char= c #\.)))
                                     (subseq header (+ mark 4)))
                          "unknown"))))
      (let ((*read-eval* nil))
        (values (read stream) version)))))

(defun %transform-attribute (attribute)
  (destructuring-bind (&key name type default-value system-default flags
                       &allow-other-keys)
      attribute
    (list :name name
          :type type
          ;; The C side preserves IUPAF_SAMEASSYSTEM as a marker; the
          ;; database has always stored the resolved value.
          :default-value (if (eq default-value :same-as-system)
                             system-default
                             default-value)
          :system-default system-default
          :flags flags)))

(defun %transform-class (class metadata)
  (destructuring-bind (&key classname format attributes &allow-other-keys) class
    (let ((vanity (assoc-value (getf metadata :vanity-alist) classname
                               :test #'string=)))
      (list :classname classname
            :format format
            :children (child-spec-from-format format)
            :override-p (and (find classname (getf metadata :override-p)
                                   :test #'string=)
                             t)
            :vanity-classname (and vanity (string-upcase vanity))
            :attributes (sort (mapcar #'%transform-attribute attributes)
                              #'string< :key #'(lambda (a) (getf a :name)))))))

(defun transform-dump (dump)
  "One dump -> (VALUES platform-section regenerated-package-names)."
  (let* ((modules (getf dump :modules))
         ;; Every class in the dump by name, wherever attribution put it,
         ;; for the :classname-claims entries.
         (all-classes (make-hash-table :test #'equal)))
    (dolist (module modules)
      (dolist (class (getf module :classes))
        (setf (gethash (getf class :classname) all-classes) class)))
    (loop for metadata in *static-metadata*
          for module = (find (getf metadata :init-function) modules
                             :key #'(lambda (m) (getf m :initializer))
                             :test #'string=)
          when module
            collect (getf metadata :package) into regenerated
          when module
            collect
            (let ((excludes (getf metadata :classname-excludes))
                  (claims (getf metadata :classname-claims)))
              (list :package (getf metadata :package)
                    :classnames
                    (append
                     (loop for class in (getf module :classes)
                           unless (find (getf class :classname) excludes
                                        :test #'string=)
                             collect (%transform-class class metadata))
                     (loop for claim in claims
                           for class = (gethash claim all-classes)
                           when class
                             collect (%transform-class class metadata)))))
              into packages
          finally (return (values (list :platform (getf dump :platform)
                                        :metadata packages)
                                  regenerated)))))

(defun classesdb-pathname ()
  (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))

(defun read-classesdbs ()
  (let ((pathname (classesdb-pathname)))
    (if (probe-file pathname)
        (with-open-file (stream pathname)
          (let ((*read-eval* nil))
            (read stream)))
        '((:platform :linux) (:platform :windows) (:platform :unix)))))

(defun update-classesdbs (current-classesdbs classesdb regenerated)
  "Merge at the PACKAGE level: regenerated packages replace their
predecessors, everything else -- an addon whose library this dump's build
did not include -- rides along from the previous file."
  (let ((platform (getf classesdb :platform)))
    (mapcar #'(lambda (existing)
                (if (eq (getf existing :platform) platform)
                    (list :platform platform
                          :metadata
                          (append (getf classesdb :metadata)
                                  (remove-if #'(lambda (package)
                                                 (find (getf package :package)
                                                       regenerated
                                                       :test #'string=))
                                             (getf existing :metadata))))
                    existing))
            current-classesdbs)))

(defun write-classesdbs (classesdbs iup-version)
  (with-open-file (stream (classesdb-pathname)
                          :direction :output :if-exists :supersede)
    (format stream ";;; generated at ~A for IUP ~A, from iup_classdump output -*-lisp-*-~%~%"
            (local-time:format-timestring
             nil (local-time:universal-to-timestamp (get-universal-time))
             :timezone local-time:+utc-zone+)
            iup-version)
    (write classesdbs :stream stream :pretty t :right-margin 100)))

(defun regenerate-from-dumps (&rest dump-pathnames)
  "Rebuild classesdb.lisp-sexp from the classes.sexp files the tecgraf-iup
release archives ship at share/iup/classes.sexp -- one per platform, any
subset, in any order."
  (let ((db (read-classesdbs))
        (version "unknown"))
    (dolist (pathname dump-pathnames)
      (multiple-value-bind (dump dump-version) (read-dump pathname)
        (setf version dump-version)
        (multiple-value-bind (section regenerated) (transform-dump dump)
          (format t "~&~A: ~{~A~^, ~}~%"
                  (getf section :platform) regenerated)
          (setf db (update-classesdbs db section regenerated)))))
    (write-classesdbs db version)
    nil))
