;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage cl-gobject-introspection-system
  (:use :common-lisp :asdf))

(in-package :cl-gobject-introspection-system)

(defsystem cl-gobject-introspection
  :description "Binding to GObjectIntrospection"
  :version "0.2"
  :author "Andrey Kutejko <andy128k@gmail.com>, Roman Klochkov <kalimehtar@mail.ru>"
  :licence "BSD"
  :depends-on (:cffi :iterate :trivial-garbage)
  :serial t
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
               (:file "src/ffi")))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-gobject-introspection))))
  (load-system '#:cl-gobject-introspection) 
  (load-system '#:cl-gobject-introspection-test))

(defsystem cl-gobject-introspection-test
  :description "Binding to GObjectIntrospection (tests)"
  :version "0.1"
  :author "Andrey Kutejko <andy128k@gmail.com>"
  :licence "LLGPL"
  :depends-on (:cl-gobject-introspection :iterate :fiveam)
  :serial t
  :components ((:file "test/package")
	       (:file "test/test1")
	       (:file "test/test-generation")
	       (:file "test/gir-test")))
