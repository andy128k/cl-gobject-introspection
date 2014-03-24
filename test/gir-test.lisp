(in-package #:gir-test)

(defvar *glib*)
(defvar *gio*)
(defvar *gtk*)

(def-suite gir :description "The GIR testing suite")

(in-suite gir)

(test ffi
      "Test the ffi function"
      (is (eql 'function
	       (progn (setf *glib* (ffi "GLib"))
		      (type-of *glib*))))
      (is (eql 'function
	       (progn (setf *gio* (ffi "Gio"))
		      (type-of *gio*))))
      (is (eql 'function
	       (progn (setf *gtk* (ffi "Gtk"))
		      (type-of *gtk*)))))

(test (enum :depends-on ffi)
      "Test the enumeration"
      (is (= 0 (call *gtk* "WindowType" :toplevel))))

(test (const :depends-on ffi)
      "Test the constant"
      (is (= 2.718282d0 (call *glib* "E")))
      (is (equal "i" (call *glib* "GINT32_FORMAT"))))

(test (function :depends-on ffi)
      "Test the function"
      ;; in-arguments, #\A
      (is-true (call *glib* 'unichar-isalpha #x41))
      ;; in-arguments, #\1
      (is-false (call *glib* 'unichar-isalpha #x31))
      ;; in-out arguments
      (is (equal '("test")
		 (let ((args '("test" "--display" ":100000.0")))
		   (multiple-value-bind (ret nargc nargv)
		       (call *gtk* 'init-check (length args) (argv-alloc args))
		     (declare (ignore ret))
		     (argv-to-list nargc nargv)))))
      ;; out arguments
      (is (equal '(nil 0.5d0 0.4d0)
		 (multiple-value-bind (ret h s v)
		     (call *gtk* 'rgb-to-hsv 0.2d0 0.3d0 0.4d0)
		   (declare (ignore h))
		   (list ret s v)))))

(def-suite struct :description "Test the struct" :in gir)

(in-suite struct)

(defparameter *entry-target* "test-entry-target")
(defparameter *entry-flags1* #x12)
(defparameter *entry-flags2* #x34)
(defvar *entry*)

(test (struct-constructor :depends-on function)
      "Test the struct constructor"
      (is (eql 'function
	       (progn
		 (setf *entry* (call *gtk* "TargetEntry" 'new
				     *entry-target* *entry-flags1* 0))
		 (type-of *entry*)))))

(test (struct-this :depends-on struct-constructor)
      "Test the struct this pointer"
      (is-true (pointerp (call *entry* :this))))

(test (struct-field :depends-on struct-constructor)
      "Test the struct field get/set"
      (is (equal *entry-target* (call *entry* :field 'target)))
      (is (= *entry-flags1* (call *entry* :field 'flags)))
      (is (= *entry-flags2* (progn (call *entry* :set-field!
					 'flags *entry-flags2*)
				   (call *entry* :field 'flags)))))

(test (struct-method :depends-on struct-this)
      "Test the struct method"
      (is (= 10000000
	     (let* ((dt1 (call *glib* "DateTime" 'new-utc 2000 1 1 0 0 0d0))
		    (dt2 (call *glib* "DateTime" 'new-utc 2000 1 1 0 0 1d1)))
	       (call dt2 'difference dt1)))))

(defvar *entry-this*)

(test (struct-foreign-obj :depends-on (and struct-method struct-field))
      "Test the struct foreign object support"
      (is-true (progn (setf *entry-this* (call *entry* :this))
		      (pointerp *entry-this*)))
      (is (equal (list *entry-target* *entry-flags2*)
		 (let ((entry (call *gtk* "TargetEntry" *entry-this*)))
		   (list (call entry :field 'target)
			 (call entry :field 'flags))))))

(test (struct-null :depends-on struct-constructor)
      "Test the null-pointer to struct return value"
      (is (eq nil (call *glib* 'main-current-source))))

(def-suite object :description "Test the object" :in gir)

(in-suite object)

(defparameter *app-id1* "test.obj.id1")
(defparameter *app-id2* "test.obj.id2")
(defvar *app*)

(test (object-constructor :depends-on (and function enum))
      "Test the object constructor"
      (is (eql 'function
	       (let ((flags (call *gio* "ApplicationFlags" :flags_none)))
		 (setf *app* (call *gio* "Application" 'new *app-id1* flags))
		 (type-of *app*)))))

(test (object-this :depends-on object-constructor)
      "Test the object this pointer"
      (is-true (pointerp (call *app* :this))))

(test (object-method :depends-on object-constructor)
      "Test the object constructor"
      (is (equal *app-id1*
		 (call *app* 'get-application-id)))
      (is (equal *app-id2*
		 (progn (call *app* 'set-application-id *app-id2*)
			(call *app* 'get-application-id)))))

(test (object-properties :depends-on object-method)
      "Test the object method"
      (is (equal *app-id2*
		 (call *app* :properties 'application-id)))
      (is (equal *app-id1*
		 (progn (call *app* :set-properties! 'application-id *app-id1*)
			(call *app* :properties 'application-id))))
      (is (equal (list *app-id1* nil)
		 (multiple-value-list
		  (call *app* :properties 'application-id 'is-registered)))))

(test (object-connect :depends-on object-method)
      "Test the object signal connect"
      (is-true (let ((is-cancelled nil)
		     (cancellable (call *gio* "Cancellable" 'new)))
		 (connect cancellable "cancelled"
			  (lambda (obj)
			    (declare (ignore obj))
			    (setf is-cancelled t)))
		 (call cancellable 'cancel)
		 is-cancelled)))

(defun main ()
  (run! 'gir))
