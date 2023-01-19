(in-package :cl-user)

(ql:quickload "cl-gobject-introspection")

(defvar *gio* (gir:ffi "Gio"))
(defvar *gtk* (gir:ffi "Gtk" "4.0"))

(defun activate (app)
  (let ((window (gir:invoke (*gtk* "ApplicationWindow" 'new) app))
        (button (gir:invoke (*gtk* "Button" 'new-with-label) "Click me")))
    (setf (gir:property button 'margin-top) 40
          (gir:property button 'margin-bottom) 40
          (gir:property button 'margin-start) 20
          (gir:property button 'margin-end) 20)
    (gir:connect button :clicked
                 (lambda (button)
                   (setf (gir:property button 'label) "Hello, world!")))
    (gir:invoke (window 'set-child) button)
    (gir:invoke (window 'show))))

(defun main ()
  (let ((app (gir:invoke (*gtk* "Application" 'new)
                         "com.example.helloworld"
                         (gir:nget *gio* "ApplicationFlags" :flags_none))))
    (gir:connect app :activate #'activate)
    (gir:invoke (app 'run) nil)))
