(in-package :gir)

(defclass base-info ()
  ((ptr :initarg :ptr :reader info-ptr)))

(defclass type-info (base-info) ())

(defclass callable-info (base-info) ())
(defclass function-info (callable-info) ())
(defclass callback-info (callable-info) ())
(defclass signal-info (callable-info) ())
(defclass vfunc-info (callable-info) ())

(defclass error-domain-info (base-info) ())

(defclass value-info (base-info) ())

(defclass field-info (base-info) ())

(defclass registered-type-info (base-info) ())
(defclass enum-info (registered-type-info) ())
(defclass interface-info (registered-type-info) ())
(defclass object-info (registered-type-info) ())
(defclass struct-info (registered-type-info) ())
(defclass union-info (registered-type-info) ())

(defclass property-info (base-info) ())

(defclass constant-info (base-info) ())

(defclass arg-info (base-info) ())

(cffi:define-foreign-type info-ffi ()
  ()
  (:actual-type :pointer))
(cffi:define-parse-method info-ffi (&key)
  (make-instance 'info-ffi))

(cffi:defcfun g-base-info-get-type info-type
  (info :pointer))

(defmethod cffi:translate-to-foreign (info (type info-ffi))
  (slot-value info 'ptr))
(defmethod cffi:translate-from-foreign (pointer (type info-ffi))
  (unless (cffi:null-pointer-p pointer)
    (make-instance (case (g-base-info-get-type pointer)
		     (:function 'function-info)
		     (:callback 'callback-info)
		     (:struct 'struct-info)
		     (:boxed 'struct-info) ;;; !!!
		     (:enum 'enum-info)
		     (:flags 'enum-info)
		     (:object 'object-info)
		     (:interface 'interface-info)
		     (:constant 'constant-info)
		     (:error-domain 'error-domain-info)
		     (:union 'union-info)
		     (:value 'value-info)
		     (:signal 'signal-info)
		     (:vfunc 'vfunc-info)
		     (:property 'property-info)
		     (:field 'field-info)
		     (:arg 'arg-info)
		     (:type 'type-info)
		     (otherwise 'base-info))
		   :ptr pointer)))

(cffi:defcfun g-base-info-ref :pointer
  (info :pointer))
(cffi:defcfun g-base-info-unref :void
  (info :pointer))

(defun info-ffi-finalize (info &optional (transfer-ownership t))
  (if info
      (let* ((pointer (slot-value info 'ptr))
	     (addr (cffi:pointer-address pointer)))
	(if (null transfer-ownership)
	    (g-base-info-ref pointer))
	(tg:finalize pointer (lambda ()
			       (g-base-info-unref (cffi:make-pointer addr))))))
  info)

(defmacro def-info-func (names &body args)
  (let* ((lisp-name (if (listp names) (car names) names))
	 (foreign-name
	   (if (listp names) (cadr names)
	       #-clisp
	       (make-symbol (concatenate 'string "G-" (symbol-name lisp-name)))
	       #+clisp
	       (intern (concatenate 'string
				    "G-"
				    (symbol-name lisp-name)) "GIR")))
	 (real-args (if (stringp (car args)) (cdr args) args))
	 (arg-names (mapcar #'first real-args)))
    `(progn
       (cffi:defcfun ,foreign-name info-ffi ,@args)
       (defun ,lisp-name ,arg-names
	 (info-ffi-finalize (,foreign-name ,@arg-names))))))

(defun info-get-type (info)
  (g-base-info-get-type (info-ptr info)))

(cffi:defcfun (info-get-name "g_base_info_get_name") :string
  (info info-ffi))

(cffi:defcfun (info-get-namespace "g_base_info_get_namespace") :string
  (info info-ffi))

(cffi:defcfun (info-is-deprecated "g_base_info_is_deprecated") :boolean
  (info info-ffi))

(cffi:defcfun (info-get-attribute "g_base_info_get_attribute") :string
  (info info-ffi)
  (name :string))

(cffi:defcfun g-base-info-iterate-attributes :boolean
  (info info-ffi)
  (iterator :pointer)
  (name :pointer)
  (value :pointer))

(defun info-get-attributes (info)
  (cffi:with-foreign-objects ((iter :pointer 4)
			      (name :pointer)
			      (value :pointer))
    (setf (cffi:mem-ref iter :pointer) (cffi:null-pointer))
    (loop
      (let ((r (g-base-info-iterate-attributes info iter name value)))
	(unless r
	  (return))
	;; foreign-string-to-lisp
	(pprint (cons name value))))))

(cffi:defcfun g-base-info-get-container info-ffi
  (info info-ffi))
(defun info-get-container (info)
  (info-ffi-finalize (g-base-info-get-container info) nil))

(cffi:defcfun (info-get-typelib "g_base_info_get_typelib") typelib-type
  (info info-ffi))

(cffi:defcfun (info-equal "g_base_info_equal") :boolean
  (info1 info-ffi)
  (info2 info-ffi))

