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

(defun object-class-build-constructor-class-function (object-class cname)
  (let* ((info (info-of object-class))
	 (function-info (object-info-find-method info cname))
	 flags)
    (if function-info
	(setf flags (function-info-get-flags function-info))
	(error "Bad FFI constructor/function name ~a" cname))
    (cond
      ((constructor? flags)
       (build-function function-info :return-interface info))
      ((class-function? flags)
       (build-function function-info))
      (t
       (error "~a is not constructor or class function" cname)))))

(defun object-class-find-function-info (object-class cname)
  (with-accessors ((info info-of) (interface-infos interface-infos-of))
      object-class
    (or (object-info-find-method info cname)
	(iter (for intf :in interface-infos)
	      (if-let ((func (interface-info-find-method intf cname)))
		(return func))))))

(defun object-class-build-method (object-class cname)
  (let* ((function-info (object-class-find-function-info object-class cname))
	 (flags (if function-info (function-info-get-flags function-info))))
    (if (and function-info (method? flags))
	(build-function function-info))))

(defun object-class-find-build-method (object-class cname)
  (with-accessors ((parent parent-of) (method-cache method-cache-of))
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

(defmethod nsget ((object object-instance) name)
  (let* ((object-class (gir-class-of object))
	 (cname (c-name name))
         (method (object-class-find-build-method object-class cname))
	 (this (this-of object)))
    (lambda (&rest args)
      (apply method (cons this args)))))

(defun gobject (gtype ptr)
  (let ((info (repository-find-by-gtype nil gtype))
	object-class)
    (if (and info (eq (info-get-type info) :object))
	(progn
	  (setf object-class (find-build-interface info))
	  (build-object-ptr object-class ptr))
        (error "gtype ~a not found in GI. Found ~a" 
               gtype (info-get-type info)))))

(cffi:define-foreign-type pobject ()
  ()
  (:documentation "pointer to GObject")
  (:actual-type :pointer)
  (:simple-parser pobject))

(defmethod cffi:translate-to-foreign (object (type pobject))
  (this-of object))

(defmethod cffi:translate-from-foreign (pointer (type pobject))
  (gobject (gtype pointer) pointer))
