(defpackage #:iup-examples.flight-booker
  (:use #:common-lisp)
  (:export #:flight-booker))

(in-package #:iup-examples.flight-booker)

(defun direction-action-cb (self text item state)
  (declare (ignore text))
  (let ((start-date (iup:get-dialog-child self "STARTDATE"))
	(end-date (iup:get-dialog-child self "ENDDATE"))
	(book-button (iup:get-dialog-child self "BOOK")))
    (when (zerop state)
      (return-from direction-action-cb iup:+default+))
    (if (= item 1)
	(setf (iup:attribute end-date :active) "NO")
	(setf (iup:attribute end-date :active) "YES"))
    (when (= item 2)
      (let ((start (iup:attribute start-date :value))
	    (end (iup:attribute end-date :value)))
	(setf (iup:attribute book-button :active) "YES")))
    iup:+default+))

(defun flight-booker ()
  (iup:with-iup ()
    (let* ((direction (iup:list :expand "HORIZONTAL" :dropdown "YES" :action 'direction-action-cb))
	   (start-date (iup:date-pick :expand "HORIZONTAL" :name "STARTDATE" :zeropreced "YES"))
	   (end-date (iup:date-pick :expand "HORIZONTAL" :name "ENDDATE" :zeropreced "YES"))
	   (book-button (iup:button :title "Book" :name "BOOK" :expand "HORIZONTAL"))
	   (vbox (iup:vbox (list direction start-date end-date book-button)
			   :gap 10
			   :margin "10x10"))
	   (dialog (iup:dialog vbox :title "Flight Booker")))
      (setf (iup:attribute direction 1) "One-way Flight"
	    (iup:attribute direction 2) "Return Flight"
	    (iup:attribute direction :value) 1
	    (iup:attribute start-date :active) "YES"
	    (iup:attribute start-date :other) end-date
	    (iup:attribute end-date :active) "NO"
	    (iup:attribute start-date :active) start-date)
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (flight-booker))
