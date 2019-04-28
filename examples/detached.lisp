(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.detached
  (:use #:common-lisp)
  (:export #:detached))

(in-package #:iup-examples.detached)

(defun detached ()
  (iup:with-iup ()
    (let* ((button1 (iup:button :title "Detach Me!"
                                :action 'button-detach-callback
                                :expand :yes
                                :handlename "detach"))
           (multi-line (iup:multi-line :expand :yes
                                       :visiblelines 5))
           (hbox (iup:hbox (list button1 multi-line) :margin "10x0"))
           (dbox (iup:detach-box hbox :orientation :vertical
                                      :detached_cb 'detached-callback
                                      :handlename "dbox"))
           (label (iup:label :title "Label"
                             :expand :vertical))
           (button2 (iup:button :title "Restore me!"
                                :expand :yes
                                :active :no
                                :action 'button-restore-callback
                                :handlename "restore"))
           (text (iup:text :expand :horizontal))
           (dialog (iup:dialog (iup:vbox (list dbox label button2 text)
                                         :margin "10x10"
                                         :gap 10)
                               :title "IupDetachBox Example"
                               :rastersize "300x300")))

      (iup:show dialog)
      (iup:main-loop))))

(defun detached-callback (handle new-parent x y)
  (setf (iup:attribute new-parent :title) "New Dialog"
        (iup:attribute (iup:handle "restore") :active) :yes
        (iup:attribute (iup:handle "detach") :active) :no)
  iup:+default+)

(defun button-restore-callback (button)
  (setf (iup:attribute (iup:handle "dbox") :restore) nil
        (iup:attribute button :active) :no
        (iup:attribute (iup:handle "detach") :active) :yes)
  iup:+default+)

(defun button-detach-callback (button)
  (setf (iup:attribute (iup:handle "dbox") :detach) nil
        (iup:attribute button :active) :no
        (iup:attribute (iup:handle "restore") :active) :yes)
  iup:+default+)

#-sbcl (detached)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (detached))
