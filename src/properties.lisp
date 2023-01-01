(in-package :gir)

(cffi:defcstruct g-param-spec
    (g-type-instance :pointer)
  (name :string)
  (flags :int)
  (spec-type :ulong)
  (owner-type :ulong))

(cffi:defcfun g-object-class-find-property :pointer 
  (obj :pointer) (name :string))

(cffi:defcfun g-object-get-property :void
  (object :pointer) (name :string) (value :pointer))

(cffi:defcfun g-object-set-property :void
  (object :pointer) (name :string) (value :pointer))

(defun property-gtype (object name)
  (let ((param (g-object-class-find-property 
                (cffi:mem-ref object :pointer)
                name)))
    (when (cffi:null-pointer-p param)
      (error "No such property name ~a" name))
    (cffi:foreign-slot-value param '(:struct g-param-spec) 'spec-type)))

(fmakunbound 'get-properties)
(defun get-properties (ptr args)
  (declare (type list args)
           (type (satisfies cffi:pointerp) ptr))
  (values-list 
   (loop :for arg :in args
	 :collect (let* ((name (c-name arg))
			 (gtype (property-gtype ptr name))
			 (gvalue (make-gvalue gtype)))
                    (g-object-get-property ptr name gvalue)
                    (gvalue->lisp/free gvalue gtype)))))

(fmakunbound 'set-properties!)
(defun set-properties! (ptr args)
  (destructuring-bind (key val &rest rest) args
    (let* ((name (c-name key))
           (gtype (property-gtype ptr name))
           (gvalue (make-gvalue gtype val)))
      (g-object-set-property ptr name gvalue)
      (cffi:foreign-free gvalue))
    (when rest (set-properties! ptr rest))))
