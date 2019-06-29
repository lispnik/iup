;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.tabs
  (:use #:common-lisp)
  (:export #:tabs))

(in-package #:iup-examples.tabs)

(defun tabs ()
  (iup:with-iup ()
    (let* ((vbox1 (iup:vbox
                   (list (iup:label :title "Inside Tab A")
                         (iup:button :title "Button A"))))
           (vbox2 (iup:vbox
                   (list (iup:label :title "Inside Tab B")
                         (iup:button :title "Button B"))))
           (tabs1 (iup:tabs (list vbox1 vbox2)))
           (vbox3 (iup:vbox
                   (list (iup:label :title "Inside C")
                         (iup:button :title "Button C"))))
           (vbox4 (iup:vbox
                   (list (iup:label :title "Inside D")
                         (iup:button :title "Button D"))))
           (tabs2 (iup:tabs (list vbox3 vbox4)))
           (box (iup:hbox (list tabs1 tabs2) :margin "10x10" :gap "10"))
           (dialog (iup:dialog box :title "IUP Tabs" :size "200x80")))
      (setf (iup:attribute vbox1 :tabtitle) "Tab A"
            (iup:attribute vbox2 :tabtitle) "Tab B"
            (iup:attribute vbox3 :tabtitle) "Tab C"
            (iup:attribute vbox4 :tabtitle) "Tab D")
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (tabs)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (tabs))
