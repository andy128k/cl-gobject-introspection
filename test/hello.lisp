(cl:defpackage #:gir-test
  (:use #:cl))
(in-package #:gir-test)

(defvar *gtk* (gir:ffi "Gtk"))

(cffi:defcallback hello :void ((btn-ptr :pointer))
  (let ((button (gir:call *gtk* "Button" btn-ptr)))
    (gir:call button :set-properties! 'label "OK"))
  (format t "Hello, pressed~%"))

(defun main ()
  (gir:call *gtk* 'init 0 (cffi:null-pointer))
  (let ((window (gir:call *gtk* "Window" 'new 
                          (gir:call *gtk* "WindowType" :toplevel)))
        (button (gir:call *gtk* "Button" 'new-with-label "Hello, world!")))
    (gir::g-signal-connect-data (gir:call window :this)
                                "destroy"
                                (cffi:foreign-symbol-pointer "gtk_main_quit")
                                (cffi:null-pointer)
                                (cffi:null-pointer)
                                0)
    (gir::g-signal-connect-data (gir:call button :this)
                                "clicked"
                                (cffi:callback hello)
                                (cffi:null-pointer)
                                (cffi:null-pointer)
                                0)
    (gir:call window 'add button)
    (gir:call window 'show-all)
    (gir:call *gtk* 'main)))
  