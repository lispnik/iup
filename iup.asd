;;;; iup.asd
;;;;
;;;; Sixteen systems, down from the thirty-one .asd files this repository
;;;; used to carry at its root: iup, iup-cffi, iup-utils, iup-all,
;;;; iup-threads, iup-classesdb, and a -cffi/wrapper pair for each of the
;;;; twelve addon libraries (cd, controls, gl, glcontrols, im, imglib,
;;;; mglplot, olecontrol, plot, scintilla, tuio, web).
;;;;
;;;; The -cffi systems are folded into their wrappers, as the sibling im
;;;; binding's im.asd did and for the same reason: the split earns its keep
;;;; as a rule about files -- x-cffi.lisp is the raw layer, x.lisp the hand
;;;; API -- but as a rule about systems it bought nothing. Nothing ever
;;;; depended on a -cffi system without also depending on its wrapper;
;;;; verified across all twelve pairs before folding.
;;;;
;;;; The addon systems use ASDF's slashy naming (iup/cd, iup/plot), which is
;;;; what requires them to be defined here: ASDF resolves "iup/cd" to
;;;; iup.asd by convention. Loading an addon still requires its C library --
;;;; iup/scintilla and iup/tuio define fine and fail at load, because
;;;; lispnik/tecgraf-iup has no CMake target for either yet.

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :components ((:static-file "classesdb.lisp-sexp")
               (:module "utils"
                :components ((:file "utils")))
               (:module "iup"
                :serial t
                :components ((:file "iup-cffi")
                             (:file "packages")
                             (:file "constants")
                             (:file "key")
                             (:file "callback")
                             (:file "global-callback")
                             (:file "classes")
                             (:file "config")
                             (:file "attributes")
                             (:file "status")
                             (:file "masks")
                             (:file "globals")
                             (:file "iup")
                             (:file "misc"))))
  :depends-on (#:cffi
               #:alexandria
               #:trivial-arguments
               #:split-sequence
               #:parse-number
               #:tecgraf-base))

;;; The addons. Each is its directory's -cffi file followed by its hand API,
;;; and each requires the matching C library at load time.

(defsystem #:iup/cd
  :description "The CD_IUP canvas contexts: draw with CD onto IUP widgets."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "cd"
  :components ((:file "cd-cffi")
               (:file "cd"))
  :depends-on (#:iup #:cd #:cffi))

(defsystem #:iup/controls
  :description "IupCells, IupMatrix and the other iupcontrols widgets."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "controls"
  :components ((:file "controls-cffi")
               (:file "controls"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/gl
  :description "IupGLCanvas."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "gl"
  :components ((:file "gl-cffi")
               (:file "gl"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/glcontrols
  :description "Widgets drawn entirely in OpenGL on an IupGLCanvasBox."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "glcontrols"
  :components ((:file "glcontrols-cffi")
               (:file "glcontrols"))
  :depends-on (#:iup #:alexandria #:cffi #:tecgraf-base))

(defsystem #:iup/im
  :description "IupLoadImage and friends: IM images in and out of IUP."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "im"
  :components ((:file "im-cffi")
               (:file "im"))
  :depends-on (#:iup #:im #:cffi))

(defsystem #:iup/imglib
  :description "IUP's built-in image library of stock icons."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "imglib"
  :components ((:file "imglib-cffi")
               (:file "imglib"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/plot
  :description "IupPlot, the 2D plotting widget over CD."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "plot"
  :components ((:file "plot-cffi")
               (:file "plot"))
  :depends-on (#:iup #:iup/controls #:cd #:cffi #:tecgraf-base))

(defsystem #:iup/mglplot
  :description "IupMglPlot, the MathGL 3D plotting widget."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "mglplot"
  :components ((:file "mglplot-cffi")
               (:file "mglplot"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/web
  :description "IupWebBrowser."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "web"
  :components ((:file "web-cffi")
               (:file "web"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/olecontrol
  :description "IupOleControl. Windows only at load time, defined everywhere."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "olecontrol"
  :components ((:file "olecontrol-cffi")
               (:file "olecontrol"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/scintilla
  :description "IupScintilla. No library in lispnik/tecgraf-iup yet; defined so that nothing here changes the day one exists."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "scintilla"
  :components ((:file "scintilla-cffi")
               (:file "scintilla"))
  :depends-on (#:iup #:cffi #:tecgraf-base))

(defsystem #:iup/tuio
  :description "IupTuioClient. No library in lispnik/tecgraf-iup yet."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "tuio"
  :components ((:file "tuio-cffi")
               (:file "tuio"))
  :depends-on (#:iup #:cffi))

(defsystem #:iup/threads
  :description "Drive IUP from other threads via IupPostMessage."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "threads"
  :components ((:file "threads"))
  ;; bordeaux-threads and lparallel are not pinned in ocicl.csv, and CI never
  ;; loads this system; a consumer supplies them.
  :depends-on (#:iup #:bordeaux-threads #:lparallel))

(defsystem #:iup/classesdb
  :description "Maintainer tool: rebuild classesdb.lisp-sexp from the classes.sexp files the tecgraf-iup release archives ship at share/iup/classes.sexp."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "classesdb"
  :components ((:file "classesdb"))
  ;; Pure Lisp. The metadata arrives as data produced by the build it
  ;; describes (written by tecgraf-iup's tools/iup_classdump.c), and this
  ;; system only transforms it:
  ;;
  ;;   (iup-classesdb:regenerate-from-dumps #p"classes-linux.sexp" ...)
  ;;
  ;; Its previous incarnation introspected a running IUP over FFI, which
  ;; needed a GUI session per platform and internal SDK symbols; see the
  ;; header of classesdb/classesdb.lisp for why that lost to this.
  :depends-on (#:alexandria #:local-time))

(defsystem #:iup/all
  :description "Everything: the widgets and every addon that has a library."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  ;; iup/scintilla and iup/tuio are absent because their C libraries do not
  ;; exist in lispnik/tecgraf-iup; iup/classesdb because it is a maintainer
  ;; tool, not part of the binding; iup/threads because its dependencies are
  ;; deliberately unpinned. iup/olecontrol keeps its #+windows guard here for
  ;; the practical reason that LOADING this aggregate is the only thing it is
  ;; for, and off Windows that load would fail on the missing DLL.
  :depends-on (#:iup
               #:iup/cd
               #:iup/controls
               #:iup/gl
               #:iup/glcontrols
               #:iup/im
               #:iup/imglib
               #:iup/mglplot
               #:iup/plot
               #+windows #:iup/olecontrol
               #:iup/web))
