(in-package #:gir-test)

(defvar *glib*)
(defvar *gio*)
(defvar *gtk*)

(def-suite gir :description "The GIR testing suite")

(in-suite gir)

(test namespace
      "Test the namespace function"
      (is (eql 'gir::namespace
	       (progn (setf *glib* (require-namespace "GLib"))
		      (type-of *glib*))))
      (is (eql 'gir::namespace
	       (progn (setf *gio* (require-namespace "Gio"))
		      (type-of *gio*))))
      (is (eql 'gir::namespace
	       (progn (setf *gtk* (require-namespace "Gtk" "3.0"))
		      (type-of *gtk*)))))

(test bad-namespace
      "Test GError signals"
      (is (eql :error-signalled
               (handler-case
                   (require-namespace "NonExistingNamespace")
                 (error (e)
                   (declare (ignore e))
                   :error-signalled)))))

(test (enum :depends-on namespace)
      "Test the enumeration"
      (is (= 0 (nget *gtk* "WindowType" :toplevel))))

(test (const :depends-on namespace)
      "Test the constant"
      (is (= 2.718282d0 (nget *glib* "E")))
      (is (equal "i" (nget *glib* "GINT32_FORMAT"))))

(test (function :depends-on namespace)
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
      (is (eql 'gir::struct-instance
	       (progn
		 (setf *entry* (invoke (*gtk* "TargetEntry" 'new)
				       *entry-target* *entry-flags1* 0))
		 (type-of *entry*)))))

(test (struct-this :depends-on struct-constructor)
      "Test the struct this pointer"
      (is-true (pointerp (gir::this-of *entry*))))

(test (struct-field :depends-on struct-constructor)
      "Test the struct field get/set"
      (is (equal *entry-target* (field *entry* 'target)))
      (is (= *entry-flags1* (field *entry* 'flags)))
      (is (= *entry-flags2* (progn (setf (field *entry* 'flags) *entry-flags2*)
				   (field *entry* 'flags)))))

(test (struct-method :depends-on struct-this)
      "Test the struct method"
      (is (= 10000000
	     (let* ((dt1 (invoke (*glib* "DateTime" 'new-utc) 2000 1 1 0 0 0d0))
		    (dt2 (invoke (*glib* "DateTime" 'new-utc) 2000 1 1 0 0 1d1)))
	       (invoke (dt2 'difference) dt1)))))

(defvar *entry-this*)

(test (struct-foreign-obj :depends-on (and struct-method struct-field))
      "Test the struct foreign object support"
      (is-true (progn (setf *entry-this* (gir::this-of *entry*))
		      (pointerp *entry-this*)))
      (is (equal (list *entry-target* *entry-flags2*)
		 (let ((entry (gir::build-struct-ptr (nget *gtk* "TargetEntry")
						     *entry-this*)))
		   (list (field entry 'target)
			 (field entry 'flags))))))

(test (struct-null :depends-on struct-constructor)
      "Test the null-pointer to struct return value"
      (is (eq nil (invoke (*glib* 'main-current-source)))))

(test (struct-allocate/free :depends-on (and struct-method struct-field))
      "Test the struct allocate/free"
      (is (equal (list 0 0 100)
		 (let ((poll-fd (allocate-struct (nget *glib* "PollFD"))))
		   (list (field poll-fd 'fd)
			 (field poll-fd 'events)
			 (progn (setf (field poll-fd 'fd) 100)
				(field poll-fd 'fd))))))
      (is-true (let ((poll-fd (allocate-struct (nget *glib* "PollFD"))))
		 (free-struct poll-fd)
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
      (is (eql 'gir::object-instance
	       (let ((flags (nget *gio* "ApplicationFlags" :handles-open)))
		 (setf *app* (invoke (*gio* "Application" 'new)
				     *app-id1* flags))
		 (type-of *app*)))))

(test (object-this :depends-on object-constructor)
      "Test the object this pointer"
      (is-true (pointerp (gir::this-of *app*))))

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

(test (object-caller-alloc :depends-on (and object-method struct-allocate/free))
      "Test the object method with caller allocated struct parameter"
      (is-true (typep (let ((file-info (invoke (*gio* "FileInfo" 'new))))
		       (multiple-value-bind (ret time-val)
			   (invoke (file-info 'get-modification-time))
			 (declare (ignore ret))
			 time-val))
		      'gir::struct-instance)))

(test (object-signal :depends-on object-constructor)
      "Test signal parameters mapping"
      (is (equal '()
		 (let* ((icon1 (gir:invoke (*gio* "ThemedIcon" 'new) "open"))
			(icon2 (gir:invoke (*gio* "ThemedIcon" 'new) "document-new"))
			(store (gir:invoke (*gio* "ListStore" 'new)
					   (gir:invoke (icon1 'get-type))))
			(calls nil))
		   (gir:connect store 'items-changed (lambda (&rest args)
						       (push args calls)))
		   (gir:invoke (store 'append) icon1)
		   (gir:invoke (store 'append) icon2)
		   calls))))

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
