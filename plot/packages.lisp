(defpackage #:iup-plot-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-plot
  (:use #:common-lisp)
  (:export #:open
	   #:plot
	   #:begin
	   #:end
	   #:with-plot
	   #:add
	   #:add-segment
	   #:add-string
	   #:load-data
	   #:insert
	   #:insert-string
	   #:insert-segment
	   #:insert-samples
	   #:sample
	   #:sample-string
	   #:sample-selection
	   #:sample-extra
	   
	   #:paint-to)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
