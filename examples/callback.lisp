(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.callback
  (:use #:common-lisp)
  (:export #:callback))

(in-package #:iup-examples.callback)

(defun callback ()
    (iup:with-iup ()
      (let* ((button
               (iup:button :title "&OK"
                           :expand "YES"
                           :tip "Exit button"
                           :action (lambda (handle)
                                     (iup:message "Message"
                                                  (lisp-implementation-version))
                                     iup:+close+)))
             (label (iup:label :title (lisp-implementation-type)))
             (vbox (iup:vbox (list label button)
                             :gap "10"
                             :margin "10x10"
                             :alignment :acenter))
             (dialog (iup:dialog vbox :title "Hello, World!")))
        (iup:show dialog)
        (iup:main-loop))))

#-sbcl (callback)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (callback))
