(in-package :gir)

(defgeneric field (object name))
(defgeneric set-field! (object name value))

(defun c-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (substitute #\_ #\- (symbol-name name))))))

(defun find-ffi-method (info name)
  (and info
       (or (object-info-find-method info name)
           (loop 
              :for i :in (object-info-get-interfaces info)
              :for j = (interface-info-find-method i name)
              :when j :return j)
           (find-ffi-method (object-info-get-parent info) name))))

(defstruct
    (object
      (:constructor make-object (class this)))
  class
  this)

(defstruct
    (object-class
      (:constructor make-object-class (info signals fields-dict)))
  info
  signals
  fields-dict)

(defun build-object (info)
  (let* ((signals (list nil))
	 (fields-dict
          (loop :for i :below (g-object-info-get-n-fields info)
             :collect
             (let ((field-info (g-object-info-get-field info i)))
               (cons (info-get-name field-info) field-info)))))
    (make-object-class info signals fields-dict)))

(defun object-class-build-constructor-class-function (object-class name)
  (let* ((info (object-class-info object-class))
	 (function-info (find-ffi-method info (c-name name)))
	 flags)
    (if function-info
	(setf flags (function-info-get-flags function-info))
	(error "Bad FFI constructor/function name ~a" name))
    (cond
      ((constructor? flags)
       (lambda (&rest args)
	 (let ((this (apply (build-function function-info
					    :return-raw-pointer t) args)))
	   (build-object-ptr object-class (object-ref-sink this)))))
      ((class-function? flags)
       (build-function function-info))
      (t
       (error "~a is not constructor or class function" name)))))

(defun object-class-build-method (object-class name)
  (let* ((info (object-class-info object-class))
	 (function-info (find-ffi-method info (c-name name)))
	 flags)
    (if function-info
	(setf flags (function-info-get-flags function-info))
	(error "Bad FFI method name ~a" name))
    (build-function function-info)))

(defun build-object-ptr (object-class this)
  (make-object object-class this))

(defun object-class-find-field (object-class name)
  (let ((fields-dict (object-class-fields-dict object-class)))
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((object-class object-class) name)
  (object-class-build-constructor-class-function object-class name))

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

(defmethod nsget ((object object) name)
  (let* ((object-class (object-class object))
	 (method (object-class-build-method object-class name))
	 (this (object-this object)))
    (lambda (&rest args)
      (apply method (cons this args)))))

(cffi:defcfun g-object-ref-sink :pointer (obj :pointer))
(cffi:defcfun g-object-unref :void (obj :pointer))

(defun object-ref-sink (obj)
  (let* ((res (g-object-ref-sink obj))
         (a (cffi:pointer-address res)))
    (tg:finalize res (lambda () (g-object-unref (cffi:make-pointer a))))
    res))

(defun gobject (gtype ptr)
  (let ((info (repository-find-by-gtype nil gtype))
	object-class)
    (if (and info (eq (info-get-type info) :object))
	(progn
	  (setf object-class (build-object info))
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
