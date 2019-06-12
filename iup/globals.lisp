(in-package #:iup)

(export '(global))

(defun (setf global) (new-value name)
  (if (typep new-value 'string)
      (cffi:with-foreign-string (ptr new-value)
	(iup-cffi::%iup-set-str-global name ptr))
      (if new-value
	  (setf (global name) (write-to-string new-value))
	  (cffi:null-pointer)))
  new-value)

(defun global (name &optional (as-type 'string))
  (check-type
   as-type (member string number cffi:foreign-pointer #+windows hinstance))
  (nth-value
   0
   (ecase as-type
     (string
      (cffi:foreign-string-to-lisp (iup-cffi::%iup-get-global name)))
     (number
      (serapeum:parse-number
       (cffi:foreign-string-to-lisp (iup-cffi::%iup-get-global name))))
     (cffi:foreign-pointer
      (iup-cffi::%iup-get-global name))
     #+windows
     (hinstance (cffi:pointer-address (iup-cffi::%iup-get-global name))))))

#+nil
(iup:with-iup ()
  (reverse
   (mapcar #'(lambda (name)
	       (if (atom name)
		   (cons name (global name))
		   (cons (car name) (global (car name) (cdr name)))))
	   '(:language
	     :version
	     :copyright
	     :driver
	     :lockloop
	     :exitloop
	     :lasterror
	     :utf8mode
	     :utf8mode_file
	     :defaultprecision
	     :defaultdecimalsymbol
	     :sb_bgcolor
	     :showmenuimages
	     :overlayscrollbar
	     :globalmenu
	     :globallayoutdlgkey
	     :globallayoutresizekey
	     :imageautoscale
	     :imagesdpi
	     :singleinstance
	     :cursorpos
	     :mousebutton
	     :shiftkey
	     :autorepeat
	     :system
	     :systemversion
	     :systemlanguage
	     :systemlocale
	     (:scrollbarsize . number)
	     :comctl32ver6
	     :gtkversion
	     :gtkdevversion
	     :motifversion
	     :motifnumber
	     :computername
	     :username
	     :exefilename
	     :gl_version
	     :gl_vendor
	     :gl_renderer
	     :xservervendor
	     :xvendorrelease
	     :fullsize
	     :screensize
	     (:screendepth . number)
	     :screendpi
	     :truecolorcanvas
	     :dwm_composition
	     :virtualscreen
	     (:monitorscount . number)
	     :monitorsinfo
	     (:hinstance . hinstance)
	     (:dll_hinstance . hinstance)
	     :appshell
	     :xdisplay
	     :xscreen
	     :dlgbgcolor
	     :dlgfgcolor
	     :menubgcolor
	     :menufgcolor
	     :txtbgcolor
	     :txtfgcolor
	     :txthlcolor
	     :linkfgcolor
	     :defaultfont
	     :defaultfontface
	     (:defaultfontsize . number)
	     :defaultfontstyle
	     :defaultbuttonpadding
	     :defaulttheme))))
