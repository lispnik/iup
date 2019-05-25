(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-im" "uiop")))

(defpackage #:iup-examples.photoview
  (:use #:common-lisp)
  (:export #:photoview))

(in-package #:iup-examples.photoview)

(defun photoview ()
  (iup:with-iup ()
    (let* ((timer (iup:timer :action_cb 'timer-callback
                             :time 30))
           (label (iup:flat-label :expand :yes :handlename "label"))
           (dialog (iup:dialog
                    label
                    :customframe :yes
                    ;;                               :opacity 255
                    :size "320x200"
                    ;;                               :topmost :yes
                    :handlename "dialog")))
      (iup:show dialog)
      (setf (iup:attribute timer :run) :yes)
      (iup:main-loop))))

(defun timer-callback (handle)
  (handler-case
      (let* ((images (alexandria:shuffle (find-photos)))
             (image (car images))
             (image-handle (iup-im:load-image (namestring image)))
             (label (iup:handle "label")))
        (setf (iup:handle "image") image-handle)
        (setf (iup:attribute label :image) "image")
        (iup:redraw (iup:handle "dialog")))
    (t (c)
      (format t "~&Error reading next image, ignoring...~A~%" c)
      (force-output)))
  iup:+default+)

(defun find-photos (&optional (pathname #p "~/Pictures/"))
  (remove-if #'(lambda (pathname)
                 (string/= (pathname-type pathname) "jpg"))
             (uiop:directory-files pathname)))

#-sbcl (photoview)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (photoview))
