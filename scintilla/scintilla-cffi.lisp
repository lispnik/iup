(in-package #:iup-scintilla-cffi)

(define-foreign-library iupscintilla
  (:unix "libiup_scintilla.so")
  (t (:default "iup_scintilla")))

(use-foreign-library iupscintilla)

(defcfun (%iup-scintilla-open "IupScintillaOpen") :void)
(defcfun (%iup-scintilla "IupScintilla") iup-cffi::ihandle)
(defcfun (%iup-scintilla-dlg "IupScintillaDlg") iup-cffi::ihandle)

(defcfun (%iup-scintilla-send-message "IupScintillaSendMessage") :pointer
  (handle iup-cffi::ihandle)
  (message :unsigned-int)
  (wparam :pointer)
  (lparam :pointer))
