(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup"
                  "iup-plot"
                  "iup-imglib"
                  "iup-threads"
                  "bordeaux-threads"
                  "cl-portaudio")))

(defpackage #:iup-examples.audio-capture
  (:use #:common-lisp))

(in-package #:iup-examples.audio-capture)

(defconstant +num-channels+ 1)
(defconstant +sample-rate+ 8000d0)
(defconstant +sample-format+ :float)
(defconstant +frames-per-buffer+ 1024)

(defun input-device-p (device-info)
  (and (zerop (pa:device-info-max-output-channels device-info))
       (> (pa:device-info-max-input-channels device-info) 0)))

(defclass model ()
  ((device-model
    :initform '()
    :accessor device-model)
   (device-selected-model
    :initform nil
    :accessor device-selected-model)))

(defvar *model* (make-instance 'model))

(defun update-device-model ()
  (setf (device-model *model*)
        (loop for device-id below (pa:get-device-count)
              for device-info = (pa:get-device-info device-id)
              when (input-device-p device-info)
                collect (cons device-id
                              (concatenate 'string
                                           (pa:host-api-info-name (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                           " "
                                           (pa:device-info-name device-info))))
        (device-selected-model *model*)
        (caar (device-model *model*))))

(defun device-list-refresh-callback (handle)
  (update-device-model)
  (loop for (device-id . name) in (device-model *model*)
        for i from 1
        do (setf (iup:attribute handle i) name))
  (unless (zerop (length (device-model *model*)))
    (setf (iup:attribute handle :value) 1))
  iup:+default+)

(defun device-list-map-callback (handle)
  (device-list-refresh-callback handle)
  iup:+default+)

(defvar *capture-thread* nil)
(defvar *capture-thread-stop* nil)
(defvar *capture-thread-stop-lock* (bt:make-lock "capture-thread-stop-lock"))

(defgeneric make-input-parameters (model))

(defmethod make-input-parameters ((model model))
  (let ((input-parameters (pa:make-stream-parameters)))
    (setf (pa:stream-parameters-device input-parameters)            (device-selected-model *model*)
          (pa:stream-parameters-channel-count input-parameters)     +num-channels+
          (pa:stream-parameters-sample-format input-parameters)     +sample-format+
          (pa:stream-parameters-suggested-latency input-parameters) 0.02d0)
    input-parameters))

;;; FIXME also need a lock around *model*, of couse

(defun start-recording-callback (handle)
  (let ((plot (iup:get-dialog-child handle "PLOT")))
    (setf (iup:attribute handle :active) :no
          (iup:attribute (iup:get-dialog-child handle "STOP") :active) :yes)
    (setf *capture-thread*
          (bt:make-thread
           (lambda ()
             (pa:with-audio-stream (pa-stream (make-input-parameters *model*) nil :frames-per-buffer +frames-per-buffer+)
               (bt:with-lock-held (*capture-thread-stop-lock*)
                 (setf *capture-thread-stop* nil))
               (iup-threads:call-with-main-loop
                (lambda ()
                  (iup-plot:begin plot t)))
               (loop with buffer = (make-array (* +frames-per-buffer+ +num-channels+) :element-type 'single-float)
                     with stop-p = nil
                     for samples from 0 by +frames-per-buffer+
                     ;; TODO update status bar with current sample count
                     do (bt:with-lock-held (*capture-thread-stop-lock*)
                          (setf stop-p *capture-thread-stop*))
                     until stop-p
                     do (progn
                          (pa:read-stream-into-array pa-stream buffer)
                          (iup-threads:call-with-main-loop
                           (lambda ()
                             (dotimes (i (length buffer))
                               (iup-plot:add plot i (aref buffer i)))))))
               (iup-threads:call-with-main-loop
                (lambda ()
                  (iup-plot:end plot))))
             (iup-threads:call-with-main-loop
              (lambda ()
                (setf (iup:attribute handle :active) :yes))))
           :name "capture-thread"))
    iup:+default+))

(defun stop-recording-callback (handle)
  (bt:with-lock-held (*capture-thread-stop-lock*)
    (setf *capture-thread-stop* t))
  (setf (iup:attribute handle :active) :no
        (iup:attribute (iup:get-dialog-child handle "START") :acive) :yes)
  iup:+default+)

(defun audio-capture ()
;;  (pa:initialize)
  (iup:open)
  ;; (iup-plot:open)
  ;; (iup-imglib:open)
  (iup-threads:start)
  (sleep 0.25)
  (let* ((device-list
           (iup:list :dropdown :yes
                     :expand :horizontal
                     :map_cb 'device-list-map-callback))
         (device-refresh
           (iup:button :title "Re&fresh"
                       :image "IUP_NavigateRefresh"
                       :action (lambda (handle)
                                 (declare (ignore handle))
                                 (device-list-refresh-callback device-list))))
         (plot
           (iup-plot:plot :expand :yes
                          :name "PLOT"))
         (start-recording
           (iup:button :title "&Record"
                       :image "IUP_MediaRecord"
                       :action 'start-recording-callback
                       :name "START"))
         (stop-recording
           (iup:button :title "&Stop"
                       :image "IUP_MediaStop"
                       :action 'stop-recording-callback
                       :name "STOP"))
         (info-label
           (iup:label :title (format nil "~A channel~:P, sample rate ~FHz, sample format ~A"
                                     +num-channels+
                                     +sample-rate+
                                     +sample-format+)
                      :expand :horizontal
                      :alignment :aright))
         (dialog
           (iup:dialog
            (iup:vbox (list (iup:hbox (list device-list device-refresh))
                            plot
                            (iup:hbox (list start-recording stop-recording (iup:vbox (list (iup:fill) info-label))))))
            :title "Audio Capture"
            :size "HALFxHALF")))
    (iup-threads:call-with-main-loop
     (lambda ()
       (iup:message "test" "herelooasdf")
       (iup:show dialog)))))

#-sbcl (audio-capture)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (audio-capture))

