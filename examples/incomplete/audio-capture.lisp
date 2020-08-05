(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-threads" "iup-plot" "bordeaux-threads" "cl-portaudio")))

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

(defvar *device-model* '())
(defvar *device-selected-model* nil)

(defun update-device-model ()
  (setf *device-model*
        (loop for device-id below (pa:get-device-count)
              for device-info = (pa:get-device-info device-id)
              when (input-device-p device-info)
                collect (cons device-id
                              (concatenate 'string
                                           (pa:host-api-info-name (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                           " "
                                           (pa:device-info-name device-info))))
        *device-selected-model* (caar *device-model*)))

(defun device-list-refresh-callback (handle)
  (update-device-model)
  (loop for (device-id . name) in *device-model*
        for i from 1
        do (setf (iup:attribute handle i)
                 name))
  (unless (zerop (length *device-model* ))
    (setf (iup:attribute handle :value) "1"))
  iup:+default+)

(defun device-list-map-callback (handle)
  (device-list-refresh-callback handle))

(defvar *capture-thread* nil)

(defun start-button-callback (handle)
  (unless *capture-thread*
    (setf *capture-thread*
          (bt:make-thread
           (lambda ()
             (let ((num-channels 1)
                   (sample-rate 8000d0)
                   (sample-format :float)
                   (frames-per-buffer 1024))
               (pa:with-audio 
                 (pa:with-default-audio-stream
                     (pa-stream num-channels num-channels
                      :sample-format sample-format
                      :sample-rate sample-rate
                      :frames-per-buffer frames-per-buffer)
                   (with-open-file (file-stream "c:/Users/mkennedy/Downloads/test.out" :direction :output :if-exists :supersede)
                     (loop with buffer = (make-array (* frames-per-buffer num-channels) :element-type 'single-float)
                           repeat 10
                           unless (pa:is-stream-stopped pa-stream)
                             do (progn
                                  (pa:read-stream-into-array pa-stream buffer)
                                  (dotimes (i (length buffer))
                                    (format file-stream "~A~%" (aref buffer i)))))))))))))
  iup:+default+)

(defun stop-button-callback (handle)
  (iup:message "test" (princ-to-string (iup:handle "PLOT")))
  iup:+default+)

(defun audio-capture ()
  (iup:start)
  (sleep 1)
  (iup:call-with-main-loop
   (lambda ()
     (iup-plot:open)
     (pa:with-audio 
       (let* ((device-list
                (iup:list :dropdown :yes
                          :expand :horizontal
                          :map_cb 'device-list-map-callback))
              (device-refresh
                (iup:button :title "Refresh"
                            :action (lambda (handle)
                                      (declare (ignore handle))
                                      (device-list-refresh-callback device-list))))
              (plot (iup-plot:plot :expand :yes
                                   ;; :name "PLOT"
                                   ))
              (start-recording (iup:button :title "Start" :action 'start-button-callback))
              (stop-recording (iup:button :title "Stop" :action 'stop-button-callback))
              (dialog (iup:dialog
                       (iup:vbox (list (iup:hbox (list device-list device-refresh))
                                       plot
                                       (iup:hbox (list start-recording stop-recording))))
                       :title "Audio Capture"
                       :size "HALFxHALF")))
         ;;          (setf (iup:handle "PLOT") plot)
         ;; (iup-plot:with-plot (plot :x-labels t)
         ;;   (loop :for label :in '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
         ;;         :for data :in '(10 20 30 40 50 60 70 80 90 0 10 20)
         ;;         :do (iup-plot:add-string plot label data)))
         (iup:show dialog))))))

#-sbcl (audio-capture)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (audio-capture))


