(defpackage #:iup-examples.hello
  (:use #:common-lisp)
  (:export #:hello))

(in-package #:iup-examples.hello)

(defun hello ()
  (iup:with-iup ()
    (let* ((label (iup:label :title (format nil "Hello, World!~%IUP ~A~%~A ~A"
                                            (iup:version)
                                            (lisp-implementation-type)
                                            (lisp-implementation-version))))
           (dialog (iup:dialog label :title "Hello, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (hello)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (hello))
