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
      (is (= 0 (nget *gtk* "WindowType" :toplevel))))

(test (const :depends-on ffi)
      "Test the constant"
      (is (= 2.718282d0 (nget *glib* "E")))
      (is (equal "i" (nget *glib* "GINT32_FORMAT"))))

(test (function :depends-on ffi)
      "Test the function"
      ;; in-arguments, #\A
      (is-true (invoke (*glib* 'unichar-isalpha) #x41))
      ;; in-arguments, #\1
      (is-false (invoke (*glib* 'unichar-isalpha) #x31))
      ;; in-out arguments
      (is (equal '("test")
		 (let ((args '("test" "--display" ":100000.0")))
		   (multiple-value-bind (ret nargv)
		       (invoke (*gtk* 'init-check) args)
		     (declare (ignore ret))
		     nargv))))
      ;; out arguments
      (is (equal '(nil 0.5d0 0.4d0)
		 (multiple-value-bind (ret h s v)
		     (invoke (*gtk* 'rgb-to-hsv) 0.2d0 0.3d0 0.4d0)
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
		 (setf *entry* (invoke (*gtk* "TargetEntry" 'new)
				       *entry-target* *entry-flags1* 0))
		 (type-of *entry*)))))

(test (struct-this :depends-on struct-constructor)
      "Test the struct this pointer"
      (is-true (pointerp (nget *entry* :this))))

(test (struct-field :depends-on struct-constructor)
      "Test the struct field get/set"
      (is (equal *entry-target* (invoke (*entry* :field) 'target)))
      (is (= *entry-flags1* (invoke (*entry* :field) 'flags)))
      (is (= *entry-flags2* (progn (invoke (*entry* :set-field!)
					   'flags *entry-flags2*)
				   (invoke (*entry* :field) 'flags)))))

(test (struct-method :depends-on struct-this)
      "Test the struct method"
      (is (= 10000000
	     (let* ((dt1 (invoke (*glib* "DateTime" 'new-utc) 2000 1 1 0 0 0d0))
		    (dt2 (invoke (*glib* "DateTime" 'new-utc) 2000 1 1 0 0 1d1)))
	       (invoke (dt2 'difference) dt1)))))

(defvar *entry-this*)

(test (struct-foreign-obj :depends-on (and struct-method struct-field))
      "Test the struct foreign object support"
      (is-true (progn (setf *entry-this* (nget *entry* :this))
		      (pointerp *entry-this*)))
      (is (equal (list *entry-target* *entry-flags2*)
		 (let ((entry (invoke (*gtk* "TargetEntry") *entry-this*)))
		   (list (invoke (entry :field) 'target)
			 (invoke (entry :field) 'flags))))))

(test (struct-null :depends-on struct-constructor)
      "Test the null-pointer to struct return value"
      (is (eq nil (invoke (*glib* 'main-current-source)))))

(test (struct-allocate/free :depends-on (and struct-method struct-field))
      "Test the struct allocate/free"
      (is (equal (list 0 0 100)
		 (let ((poll-fd (invoke (*glib* "PollFD" :allocate))))
		   (list (invoke (poll-fd :field) 'fd)
			 (invoke (poll-fd :field) 'events)
			 (progn (invoke (poll-fd :set-field!) 'fd 100)
				(invoke (poll-fd :field) 'fd))))))
      (is-true (let ((poll-fd (invoke (*glib* "PollFD" :allocate))))
		 (invoke (poll-fd :free))
		 t)))

(def-suite object :description "Test the object" :in gir)

(in-suite object)

(defparameter *app-id1* "test.obj.id1")
(defparameter *app-id2* "test.obj.id2")
(defvar *app*)

(test (class-function :depends-on function)
      "Test the class function"
      (is (eq t (invoke (*gio* "Application" 'id-is-valid) *app-id1*))))

(test (object-constructor :depends-on (and function enum))
      "Test the object constructor"
      (is (eql 'gir::object
	       (let ((flags (nget *gio* "ApplicationFlags" :flags_none)))
		 (setf *app* (invoke (*gio* "Application" 'new)
				     *app-id1* flags))
		 (type-of *app*)))))

(test (object-this :depends-on object-constructor)
      "Test the object this pointer"
      (is-true (pointerp (gir::object-this *app*))))

(test (object-method :depends-on object-constructor)
      "Test the object constructor"
      (is (equal *app-id1*
		 (invoke (*app* 'get-application-id))))
      (is (equal *app-id2*
		 (progn (invoke (*app* 'set-application-id) *app-id2*)
			(invoke (*app* 'get-application-id))))))

(test (object-properties :depends-on object-method)
      "Test the object method"
      (is (equal *app-id2*
		 (property *app* 'application-id)))
      (is (equal *app-id1*
		 (progn (setf (property *app* 'application-id) *app-id1*)
			(property *app* 'application-id)))))

(test (object-connect :depends-on object-method)
      "Test the object signal connect"
      (is-true (let ((is-cancelled nil)
		     (cancellable (invoke (*gio* "Cancellable" 'new))))
		 (connect cancellable "cancelled"
			  (lambda (obj)
			    (declare (ignore obj))
			    (setf is-cancelled t)))
		 (invoke (cancellable 'cancel))
		 is-cancelled)))

(in-suite gir)

(test (array :depends-on object-method)
      "Test the array parameter/return"
      (is (equal "abc/de/f"
		 (invoke (*glib* 'build-filenamev) '("abc" "de" "f"))))
      (is (equal '("a" "b" "c")
		 (let ((regex (invoke (*glib* "Regex" 'new) "_"
				      (nget *glib* "RegexCompileFlags" :multiline)
				      (nget *glib* "RegexMatchFlags" :newline_lf))))
		   (invoke (regex 'split) "a_b_c"
			   (nget *glib* "RegexMatchFlags" :newline_lf))))))

(defun main ()
  (run! 'gir))
