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
