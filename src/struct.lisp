(in-package :gir)

(defstruct
    (struct
      (:constructor make-struct (class this)))
  class
  this)

(defclass struct-instance ()
  ((class :initarg :class :reader struct-class-of)
   (this :initarg :this :accessor this-of)))

(defclass struct-class ()
  ((info :initarg :info :reader info-of)
   signals
   (fields-dict :reader fields-dict-of)
   (constructor-cache :reader constructor-cache-of)
   (method-cache :reader method-cache-of)))

(defmethod print-object ((struct-class struct-class) s)
  (format s "#S<~a>" (info-get-name (info-of struct-class))))

(defmethod shared-initialize :after ((struct-class struct-class) slot-names
				     &key)
  (declare (ignore slot-names))
  (with-slots (info signals fields-dict constructor-cache method-cache)
      struct-class
    (setf signals (list nil)
	  fields-dict (iter (for field-info :in (struct-info-get-fields info))
			    (collect (cons (info-get-name field-info) field-info)))
	  constructor-cache (make-hash-table :test #'equal)
	  method-cache (make-hash-table :test #'equal))))

(defmethod build-interface ((info struct-info))
  (make-instance 'struct-class :info info))

(defun %allocate-struct (struct-class)
  (let* ((info (info-of struct-class))
	 (size (struct-info-get-size info)))
    (cffi:foreign-alloc :int8 :initial-element 0 :count size)))

(defun allocate-struct (struct-class)
  (build-struct-ptr struct-class (%allocate-struct struct-class)))

(defun struct-class-build-constructor (struct-class name)
  (let* ((info (info-of struct-class))
	 (function-info (struct-info-find-method info (c-name name)))
	 (flags (if function-info (function-info-get-flags function-info))))
    (unless (and function-info (constructor? flags))
      (error "Bad FFI constructor name ~a" name))
    (build-function function-info :return-interface info)))

(defun struct-class-build-method (struct-class name)
  (let* ((info (info-of struct-class))
	 (function-info (struct-info-find-method info (c-name name)))
	 (flags (if function-info (function-info-get-flags function-info))))
    (unless (and function-info (method? flags))
      (error "Bad FFI constructor name ~a" name))
    (build-function function-info)))

(defun build-struct-ptr (struct-class this)
  (make-instance 'struct-instance :class struct-class :this this))

(defun struct-class-find-field (struct-class name)
  (let ((fields-dict (fields-dict-of struct-class)))
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((struct-class struct-class) name)
  (let* ((constructor-cache (constructor-cache-of struct-class))
	 (cname (c-name name)))
    (ensure-gethash cname constructor-cache
		    (struct-class-build-constructor struct-class cname))))

(defmethod field ((struct struct-instance) name)
  (let* ((struct-class (struct-class-of struct))
	 (field-info (struct-class-find-field struct-class name)))
    (gir.field:get (this-of struct) field-info)))

(defmethod set-field! ((struct struct-instance) name value)
  (let* ((struct-class (struct-class-of struct))
	 (field-info (struct-class-find-field struct-class name)))
    (gir.field:set (this-of struct) field-info value))
  value)

(defun (setf field) (value struct name)
  (set-field! struct name value))

(defun free-struct (struct)
  (let ((this (this-of struct)))
    (if (cffi:null-pointer-p this)
	(error "Double free")
	(progn (cffi:foreign-free this)
	       (setf (this-of struct) (cffi:null-pointer))))))

(defmethod nsget ((struct struct-instance) name)
  (let* ((struct-class (struct-class-of struct))
	 (method-cache (method-cache-of struct-class))
	 (cname (c-name name))
	 (method (ensure-gethash cname method-cache
				 (struct-class-build-method struct-class cname))))
    (lambda (&rest args)
      (apply method (cons (this-of struct) args)))))
