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
