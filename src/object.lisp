(in-package :gir)

(defgeneric field (object name))
(defgeneric set-field! (object name value))

(defun c-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (substitute #\_ #\- (symbol-name name))))))

(defstruct
    (object
      (:constructor make-object (class this)))
  class
  this)

(defstruct
    (object-class
      (:constructor make-object-class (parent info interface-infos signals
				       fields-dict function-cache
				       method-cache)))
  parent
  info
  interface-infos
  signals
  fields-dict
  function-cache
  method-cache)

(defun build-object-class (info)
  (let* ((parent
	  (let ((parent-info (object-info-get-parent info)))
	    (if parent-info
		(find-build-interface parent-info)
		nil)))
	 (signals (list nil))
	 (fields-dict
          (loop :for i :below (g-object-info-get-n-fields info)
             :collect
             (let ((field-info (g-object-info-get-field info i)))
               (cons (info-get-name field-info) field-info)))))
    (make-object-class parent info (object-info-get-interfaces info)
		       signals fields-dict
		       (make-hash-table :test 'equal)
		       (make-hash-table :test 'equal))))

(defun object-class-build-constructor-class-function (object-class cname)
  (let* ((info (object-class-info object-class))
	 (function-info (object-info-find-method info cname))
	 flags)
    (if function-info
	(setf flags (function-info-get-flags function-info))
	(error "Bad FFI constructor/function name ~a" cname))
    (cond
      ((constructor? flags)
       (let ((constructor (build-function function-info :return-raw-pointer t)))
	 (lambda (&rest args)
	   (let ((this (apply constructor args)))
	     (object-setup-gc (build-object-ptr object-class this)
			      :everything)))))
      ((class-function? flags)
       (build-function function-info))
      (t
       (error "~a is not constructor or class function" cname)))))

(defun object-class-find-function-info (object-class cname)
  (let* ((info (object-class-info object-class))
	 (interface-infos (object-class-interface-infos object-class)))
    (or (object-info-find-method info cname)
	(loop
	   :for intf :in interface-infos
	   :for func = (interface-info-find-method intf cname)
	   :when func :return func))))

(defun object-class-build-method (object-class cname)
  (let* ((function-info (object-class-find-function-info object-class cname))
	 (flags (if function-info (function-info-get-flags function-info))))
    (if (and function-info (method? flags))
	(build-function function-info))))

(defun object-class-find-build-method (object-class cname)
  (let* ((parent (object-class-parent object-class))
	 (method-cache (object-class-method-cache object-class))
         (method (gethash cname method-cache)))
    (if method
	method
	(progn
	  (setf method (object-class-build-method object-class cname))
	  (if method
	      (setf (gethash cname method-cache) method)
	      (if parent
		  (setf method (object-class-find-build-method parent cname))
		  (error "Bad FFI method name ~a" cname)))))))

(defun build-object-ptr (object-class this)
  (make-object object-class this))

(defun object-class-find-field (object-class name)
  (let ((fields-dict (object-class-fields-dict object-class)))
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((object-class object-class) name)
  (let* ((function-cache (object-class-function-cache object-class))
	 (cname (c-name name))
	 (function (gethash cname function-cache)))
    (if function
	function
	(setf (gethash cname function-cache)
	      (object-class-build-constructor-class-function object-class cname)))))

(defmethod field ((object object) name)
  (let* ((object-class (object-class object))
	 (field-info (object-class-find-field object-class name)))
    (gir.field:get (object-this object) field-info)))

(defmethod set-field! ((object object) name value)
  (let* ((object-class (object-class object))
	 (field-info (object-class-find-field object-class name)))
    (gir.field:set (object-this object) field-info value)))

(defun property (object name)
  (get-properties (object-this object) (list name)))

(defun (setf property) (value object name)
  (set-properties! (object-this object) (list name value)))

(cffi:defcfun g-object-is-floating :boolean (obj :pointer))
(cffi:defcfun g-object-ref-sink :pointer (obj :pointer))
(cffi:defcfun g-object-ref :pointer (obj :pointer))
(cffi:defcfun g-object-unref :void (obj :pointer))

(defun object-setup-gc (object transfer)
  (let* ((this (object-this object))
	 (floating? (g-object-is-floating this))
         (a (cffi:pointer-address this)))
    (if (eq transfer :everything)
	(if floating? (g-object-ref-sink this))
	(g-object-ref this))
    (tg:finalize this (lambda () (g-object-unref (cffi:make-pointer a)))))
  object)

(defmethod nsget ((object object) name)
  (let* ((object-class (object-class object))
	 (cname (c-name name))
         (method (object-class-find-build-method object-class cname))
	 (this (object-this object)))
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
  (object-this object))

(defmethod cffi:translate-from-foreign (pointer (type pobject))
  (gobject (gtype pointer) pointer))
