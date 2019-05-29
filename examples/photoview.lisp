(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-im" "im" "uiop")))

(defpackage #:iup-examples.photoview
  (:use #:common-lisp)
  (:export #:photoview))

(in-package #:iup-examples.photoview)

(defun photoview ()
  (iup:with-iup ()
    (let* ((timer (iup:timer :action_cb 'timer-callback :time 2000))
           (label (iup:flat-label :expand :yes :handlename "label"))
           (dialog (iup:dialog
                    label
                    :customframe :yes
                    :opacity 255
;;                    :size "320x200"
                    ;;:topmost :yes
                    :handlename "dialog")))
      (iup:show dialog)
      (timer-callback nil)
      (setf (iup:attribute timer :run) :yes)
      (iup:main-loop)
      (setf (iup:attribute timer :run) :no))))

(defun timer-callback (handle)
  (handler-case
      (let* ((image-pathnames (alexandria:shuffle (find-photos)))
             (image-pathname (car image-pathnames))
             (im-image-source (iup-im:load- ))
             (image-handle (iup-im:load-image image-pathname))
             (label (iup:handle "label"))
             (dialog (iup:handle "dialog")))
        (when (iup:handle "image")
          (iup:destroy (iup:handle "image")))
        (setf (iup:handle "image") image-handle
              (iup:attribute label :image) "image"
              (iup:attribute dialog :size) (iup:attribute (iup:handle "image") :size))
        (iup:refresh dialog))
    (t (c)
      (format t "~&Error reading next image, ignoring... ~A~%" c)
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
