(cl:defpackage #:gir-test
  (:use #:cl))
(in-package #:gir-test)

(defvar *gtk* (gir:ffi "Gtk"))

(cffi:defcallback hello :void ((btn-ptr :pointer))
  (let ((button (funcall (gir:nget *gtk* "Button") btn-ptr)))
    (setf (gir:property button 'label) "OK"))
  (format t "Hello, pressed~%"))

(defun main ()
  (gir:invoke (*gtk* 'init) nil)
  (let ((window (gir:invoke (*gtk* "Window" 'new)
			    (gir:nget *gtk* "WindowType" :toplevel)))
        (button (gir:invoke (*gtk* "Button"
                                   'new-with-label)
                            "Hello, world!")))
    (gir::g-signal-connect-data (gir:object-this window)
                                "destroy"
                                (cffi:foreign-symbol-pointer "gtk_main_quit")
                                (cffi:null-pointer)
                                (cffi:null-pointer)
                                0)
    #+nil
    (gir::g-signal-connect-data (gir:call button :this)
                                "clicked"
                                (cffi:callback hello)
                                (cffi:null-pointer)
                                (cffi:null-pointer)
                                0)
    (gir:connect button :clicked 
                 (lambda (button)
                   (setf (gir:property button 'label) "OK")))
    (gir:invoke (window 'add) button)
    (gir:invoke (window 'show-all))
    (gir:invoke (*gtk* 'main))))
