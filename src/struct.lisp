(in-package :gir)

(defstruct
    (struct
      (:constructor make-struct (class this)))
  class
  this)

(defstruct
    (struct-class
      (:constructor make-struct-class (info signals fields-dict)))
  info
  signals
  fields-dict)

(defun build-struct (info)
  (let* ((signals (list nil))
         (fields-dict
          (loop :for field-info :in (struct-info-get-fields info)
             :collect (cons (info-get-name field-info) field-info))))
    (make-struct-class info signals fields-dict)))

(defun allocate-struct (struct-class)
  (let* ((info (struct-class-info struct-class))
	 (size (struct-info-get-size info))
	 (this (cffi:foreign-alloc :int8 :initial-element 0 :count size)))
    (build-struct-ptr struct-class this)))

(defun struct-class-build-constructor (struct-class name)
  (let* ((info (struct-class-info struct-class))
	 (function-info (struct-info-find-method info (c-name name)))
	 (flags (function-info-get-flags function-info)))
    (unless (and function-info (constructor? flags))
      (error "Bad FFI constructor name ~a" name))
    (build-function function-info :return-raw-pointer t)))

(defun struct-class-build-method (struct-class name)
  (let* ((info (struct-class-info struct-class))
	 (function-info (struct-info-find-method info (c-name name)))
	 (flags (function-info-get-flags function-info)))
    (unless (and function-info (method? flags))
      (error "Bad FFI constructor name ~a" name))
    (build-function function-info)))

(defun build-struct-ptr (struct-class this)
  (make-struct struct-class this))

(defun struct-class-find-field (struct-class name)
  (let ((fields-dict (struct-class-fields-dict struct-class)))
    (cdr (or (assoc (c-name name) fields-dict :test #'string=)
	     (error "Bad FFI field name ~a" name)))))

(defmethod nsget ((struct-class struct-class) name)
  (let* ((constructor (struct-class-build-constructor struct-class name)))
    (lambda (&rest args)
      (build-struct-ptr struct-class (apply constructor args)))))

(defmethod field ((struct struct) name)
  (let* ((struct-class (struct-class struct))
	 (field-info (struct-class-find-field struct-class name)))
    (gir.field:get (struct-this struct) field-info)))

(defmethod set-field! ((struct struct) name value)
  (let* ((struct-class (struct-class struct))
	 (field-info (struct-class-find-field struct-class name)))
    (gir.field:set (struct-this struct) field-info value))
  value)

(defun (setf field) (value struct name)
  (set-field! struct name value))

(defun free-struct (struct)
  (let ((this (struct-this struct)))
    (if (cffi:null-pointer-p this)
	(error "Double free")
	(progn (cffi:foreign-free this)
	       (setf (struct-this struct) (cffi:null-pointer))))))

(defmethod nsget ((struct struct) name)
  (let* ((struct-class (struct-class struct))
	 (method (struct-class-build-method struct-class name)))
    (lambda (&rest args)
      (apply method (cons (struct-this struct) args)))))
