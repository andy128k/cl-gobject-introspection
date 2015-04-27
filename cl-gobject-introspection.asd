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
  :depends-on (:alexandria :cffi :iterate :trivial-garbage)
  :serial t
  :in-order-to ((test-op (load-op :cl-gobject-introspection-test)))
  :perform (test-op (o c) (uiop:symbol-call :gir-test :main))
  :components ((:file "src/package")
               (:file "src/init" :depends-on ("src/package"))
               (:file "src/typelib" :depends-on ("src/init"))
               (:file "src/types" :depends-on ("src/package"))
               (:file "src/baseinfo" :depends-on ("src/types"))
               (:file "src/repository" :depends-on ("src/baseinfo"))
               (:file "src/ffi" :depends-on ("src/baseinfo"))
               (:file "src/function" :depends-on ("src/ffi"))
               (:file "src/field" :depends-on ("src/ffi"))
               (:file "src/object" :depends-on ("src/field"))
               (:file "src/gvalue" :depends-on ("src/init"))
               (:file "src/signal" :depends-on ("src/init"))
               (:file "src/properties" :depends-on ("src/init"))
               (:file "src/enum" :depends-on ("src/ffi"))
               (:file "src/struct" :depends-on ("src/field"))
	       (:file "src/utilities" :depends-on ("src/package"))))
