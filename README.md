# IUP

## Design Goals

* safe arument checking
* similar to Lua API
* expose attributes as docs via keyword arguments
* nice event callbacks like http://orthecreedence.github.io/cl-async/documentation

e.g. 
```
(as:start-event-loop
  (lambda ()
    (format t "Exiting event loop.~%")))
```
* attribute names should not differ from docs

* IUP functions that have equivalents in Common Lisp will not be included (e.g. all the Iup*f ones that take format strings)

## callbacks

kinds of:


IDLE_ACTION        int function(void)
GLOBALCTRLFUNC_CB  void function(int c)

MAP_CB             int function(Ihandle *ih)
UNMAP_CB
DESTROY_CB
GETFOCUS_CB
KILLFOCUS_CB
ENTERWINDOW_CB
LEAVEWINDOW_CB

K_ANY              int function(Ihandle *ih, int c)
HELP_CB            void function(Ihandle *ih, int c)

ACTION             int function(Ihandle *ih) NOTE: in general (some controls receive more parameters)


Others:

int function(Ihandle* ih, int button, int pressed); [in C]

(defmacro map-callback (handle &body body)
	(cffi:get-callback (cffi:defcallback ,(gensym) ((handle iup-cffi::ihandle) :int
		,@body)

(map-callback (handle) 
  (print handle)
  -1)
  
