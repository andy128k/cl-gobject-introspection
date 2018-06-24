(in-package :gir)

(defgeneric field (object name))
(defgeneric set-field! (object name value))

(defun c-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (substitute #\_ #\- (symbol-name name))))))

(defclass object-instance ()
  ((class :initarg :class :reader gir-class-of)
   (this :initarg :this :reader this-of)))

(defclass object-class ()
  ((parent :initarg :parent :reader parent-of)
   (info :initarg :info :reader info-of)
   (interface-infos :reader interface-infos-of)
   (signals :reader signals-of)
   (fields-dict :reader fields-dict-of)
   (function-cache :reader function-cache-of)
   (method-cache :reader method-cache-of)))

(defmethod print-object ((obj-cls object-class) s)
  (format s "#O<~a>" (info-get-name (info-of obj-cls))))

(defmethod shared-initialize :after ((object-class object-class) slot-names
				     &key info)
  (declare (ignore slot-names))
  (with-slots ((object-info info) parent interface-infos signals
	       fields-dict function-cache method-cache)
      object-class
    (setf object-info info
	  parent (if-let ((parent-info (object-info-get-parent info)))
		   (find-build-interface parent-info)
		   nil)
	  interface-infos (object-info-get-interfaces info)
	  signals (list nil)
	  fields-dict (iter (for field-info :in (object-info-get-fields info))
			    (collect (cons (info-get-name field-info) field-info)))
	  function-cache (make-hash-table :test #'equal)
	  method-cache (make-hash-table :test #'equal))))

(defmethod build-interface ((info object-info))
  (make-instance 'object-class :info info))

(defmethod build-interface ((info interface-info))
  (make-instance 'interface-desc :info info))

(defun object-class-get-constructor-class-function-info (object-class cname)
  (let* ((info (info-of object-class))
	 (function-info (object-info-find-method info cname))
	 flags)
    (if function-info
	(setf flags (function-info-get-flags function-info))
	(error "Bad FFI constructor/function name ~a" cname))
    (cond
      ((constructor? flags)
       (values function-info info))
      ((class-function? flags)
       (values function-info nil))
      (t
       (error "~a is not constructor or class function" cname)))))

(defun object-class-build-constructor-class-function (object-class cname)
  (multiple-value-bind (function-info return-interface)
      (object-class-get-constructor-class-function-info object-class cname)
    (build-function function-info :return-interface return-interface)))

(defun object-class-find-function-info (object-class cname)
  (with-accessors ((info info-of) (interface-infos interface-infos-of))
      object-class
    (or (object-info-find-method info cname)
	(iter (for intf :in interface-infos)
	      (if-let ((func (interface-info-find-method intf cname)))
		(return func))))))

(defun object-class-find-method-function-info (object-class cname)
  (if-let ((function-info (object-class-find-function-info object-class cname)))
    (when (method? (function-info-get-flags function-info))
      function-info)
    (if-let ((parent (parent-of object-class)))
      (object-class-find-method-function-info parent cname))))

(defun object-class-build-method (object-class cname)
  (if-let ((func-info (object-class-find-method-function-info object-class cname)))
    (and func-info (build-function func-info))))

(defun object-class-find-build-method (object-class cname)
  (with-accessors ((method-cache method-cache-of))
      object-class
    (ensure-gethash-unless-null cname method-cache
				(object-class-build-method object-class cname)
				(error "Bad FFI method name ~a" cname))))

(defun build-object-ptr (object-class this)
  (make-instance 'object-instance :class object-class :this this))

(defun object-class-find-field (object-class name)
  (with-accessors ((fields-dict fields-dict-of))
      object-class
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((object-class object-class) name)
  (let ((cname (c-name name)))
    (ensure-gethash-unless-null cname (function-cache-of object-class)
				(object-class-build-constructor-class-function object-class cname)
				(error "Bad FFI constructor/class function name ~a" name))))

(defmethod field ((object object-instance) name)
  (let* ((object-class (gir-class-of object))
	 (field-info (object-class-find-field object-class name)))
    (gir.field:get (this-of object) field-info)))

(defmethod set-field! ((object object-instance) name value)
  (let* ((object-class (gir-class-of object))
	 (field-info (object-class-find-field object-class name)))
    (gir.field:set (this-of object) field-info value)))

(defun property (object name)
  (get-properties (this-of object) (list name)))

(defun (setf property) (value object name)
  (set-properties! (this-of object) (list name value)))

(cffi:defcfun g-object-is-floating :boolean (obj :pointer))
(cffi:defcfun g-object-ref-sink :pointer (obj :pointer))
(cffi:defcfun g-object-ref :pointer (obj :pointer))
(cffi:defcfun g-object-unref :void (obj :pointer))

(defun object-setup-gc (object transfer)
  (let* ((this (this-of object))
	 (floating? (g-object-is-floating this))
         (a (cffi:pointer-address this)))
    (if (eq transfer :everything)
	(if floating? (g-object-ref-sink this))
	(g-object-ref this))
    (tg:finalize this (lambda () (g-object-unref (cffi:make-pointer a)))))
  object)

(defgeneric find-build-method (object-class cname))
(defmethod find-build-method ((object-class object-class) cname)
  (object-class-find-build-method object-class cname))

(defmethod nsget ((object object-instance) name)
  (let* ((object-class (gir-class-of object))
	 (cname (c-name name))
         (method (find-build-method object-class cname))
	 (this (this-of object)))
    (lambda (&rest args)
      (apply method (cons this args)))))

(defclass fake-object-class ()
  ((name :initarg :name)
   (gtype :initarg :gtype)))

(defmethod print-object ((self fake-object-class) s)
  (with-slots (name gtype) self
    (let ((interface-p (= (g-type-fundamental gtype) 8)))
      (format s "#~C<fake.~a>" (if interface-p #\I #\O)
              name))))

(defvar *fake-object-classes* (make-hash-table))

(defun find-fake-object-class (gtype)   ; when gtype is not in g-i
  (let ((fundamental (g-type-fundamental gtype)))
    (when (or (= fundamental 8) (= fundamental 80))
      (assert (/= gtype 80))
      (or (gethash gtype *fake-object-classes*)
          (setf (gethash gtype *fake-object-classes*)
                (make-instance 'fake-object-class
                  :gtype gtype
                  :name (cffi:foreign-funcall "g_type_name"
                                              :ulong gtype :string)))))))

(defun gobject (gtype ptr)
  (let* ((info (repository-find-by-gtype nil gtype))
	 (info-type (and info (info-get-type info)))
         (object-class  (if (null info) (find-fake-object-class gtype))))
    (when object-class
      (return-from gobject (build-object-ptr object-class ptr)))
    (if (member info-type '(:object :struct))
	(let ((object-class (find-build-interface info)))
	  (if (eq info-type :object)
	    (build-object-ptr object-class ptr)
	    (build-struct-ptr object-class ptr)))
        (error "gtype ~a not found in GI. Found ~a" 
               gtype info-type))))

(cffi:define-foreign-type pobject ()
  ()
  (:documentation "pointer to GObject")
  (:actual-type :pointer)
  (:simple-parser pobject))

(defmethod cffi:translate-to-foreign (object (type pobject))
  (this-of object))

(defmethod cffi:translate-from-foreign (pointer (type pobject))
  (if (cffi:null-pointer-p pointer)
      nil
      (gobject (gtype pointer) pointer)))

(defmethod nsget-desc ((object-class object-class) name)
  (multiple-value-bind (function-info return-interface)
      (object-class-get-constructor-class-function-info object-class (c-name name))
    (build-callable-desc function-info :return-interface return-interface)))

(defmethod list-fields-desc ((object-class object-class))
  (let ((fields-dict (fields-dict-of object-class)))
    (iter (for (name . field-info) :in fields-dict)
	  (collect (build-variable-desc name (field-info-get-type field-info))))))

(defmethod get-field-desc ((object-class object-class) name)
  (let* ((cname (c-name name))
	 (field-info (object-class-find-field object-class cname)))
    (build-variable-desc cname (field-info-get-type field-info))))

(defmethod list-properties-desc ((object-class object-class))
  (let ((info (info-of object-class)))
    (iter (for prop-info :in (object-info-get-properties info))
	  (collect (build-variable-desc (info-get-name prop-info)
					(property-info-get-type prop-info))))))

(defmethod get-property-desc ((object-class object-class) name)
  (let ((cname (c-name name))
	(props-desc (list-properties-desc object-class)))
    (iter (for prop-desc :in props-desc)
	  (when (equal cname (name-of prop-desc))
	    (return prop-desc)))
    (error "~a is not property name" cname)))

(defmethod list-methods-desc ((object-class object-class))
  (let ((info (info-of object-class)))
    (iter (for method-info :in (object-info-get-methods info))
	  (when (method? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info))))))

(defmethod get-method-desc ((object-class object-class) name)
  (let* ((cname (c-name name))
	 (func-info (object-class-find-method-function-info object-class cname)))
    (if func-info
	(build-callable-desc func-info)
	(error "~a is not method name" cname))))

(defmethod list-class-functions-desc ((object-class object-class))
  (let ((info (info-of object-class)))
    (iter (for method-info :in (object-info-get-methods info))
	  (when (class-function? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info))))))

(defmethod list-constructors-desc ((object-class object-class))
  (let ((info (info-of object-class)))
    (iter (for method-info :in (object-info-get-methods info))
	  (when (constructor? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info :return-interface info))))))

(defmethod list-signals-desc ((object-class object-class))
  (let ((info (info-of object-class)))
    (iter (for signal-info :in (object-info-get-signals info))
	  (collect (build-callable-desc signal-info)))))

(defun object-class-find-signal-info (object-class cname)
  (let ((object-info (info-of object-class)))
    (or (object-info-find-signal object-info cname)
	(iter (for intf-info :in (object-info-get-interfaces object-info))
	      (when-let ((signal-info (interface-info-find-signal intf-info cname)))
		(return signal-info)))
	(when-let ((parent (parent-of object-class)))
	  (object-class-find-signal-info parent cname)))))

(defmethod get-signal-desc ((object-class object-class) name)
  (let* ((cname (c-name name))
	 (signal-info (object-class-find-signal-info object-class cname)))
    (if signal-info
	(build-callable-desc signal-info)
	(error "~a is not signal name" cname))))

(defclass interface-desc ()
  ((info :initarg :info :reader info-of)))

(defmethod find-build-method ((object-class interface-desc) cname)
  (let ((func-info (interface-info-find-method
		    (info-of object-class) cname)))
    (and func-info (build-function func-info))))

(defmethod print-object ((interface-desc interface-desc) s)
  (format s "I<~a>" (info-get-name (info-of interface-desc))))

(defun list-interfaces-desc (object-class)
  (iter (for intf-info :in (interface-infos-of object-class))
	(collect (make-instance 'interface-desc :info intf-info))))

(defmethod list-properties-desc ((interface-desc interface-desc))
  (let ((info (info-of interface-desc)))
    (iter (for prop-info :in (interface-info-get-properties info))
	  (collect (build-variable-desc (info-get-name prop-info)
					(property-info-get-type prop-info))))))

(defmethod list-methods-desc ((interface-desc interface-desc))
  (let ((info (info-of interface-desc)))
    (iter (for method-info :in (interface-info-get-methods info))
	  (when (method? (function-info-get-flags method-info))
	    (collect (build-callable-desc method-info))))))

(defmethod list-signals-desc ((interface-desc interface-desc))
  (let ((info (info-of interface-desc)))
    (iter (for signal-info :in (interface-info-get-signals info))
	  (collect (build-callable-desc signal-info)))))
