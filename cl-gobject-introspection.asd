;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage cl-gobject-introspection-system
  (:use :common-lisp :asdf))

(in-package :cl-gobject-introspection-system)

(defsystem cl-gobject-introspection
  :description "Binding to GObjectIntrospection"
  :version "0.3"
  :author "Andrey Kutejko <andy128k@gmail.com>, Roman Klochkov <kalimehtar@mail.ru>"
  :licence "BSD"
  :depends-on (:cffi :iterate :trivial-garbage)
  :serial t
  :in-order-to ((test-op (load-op :cl-gobject-introspection-test)))
  :perform (test-op (o c) (uiop:symbol-call :gir-test :main))
  :components ((:file "src/package")
               (:file "src/init")
               (:file "src/typelib")
               (:file "src/types")
               (:file "src/baseinfo")
               (:file "src/repository")
               (:file "src/function")
               (:file "src/field")
               (:file "src/object")
               (:file "src/gvalue")
               (:file "src/signal")
               (:file "src/properties")
               (:file "src/enum")
               (:file "src/struct")
               (:file "src/ffi")
	       (:file "src/utilities")))
