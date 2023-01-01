(in-package :gir)

(defstruct typelib
  ptr)

(cffi:define-foreign-type typelib-type ()
  ()
  (:actual-type :pointer))
(cffi:define-parse-method typelib-type (&key)
  (make-instance 'typelib-type))

(defmethod cffi:translate-to-foreign (tlb (type typelib-type))
  (slot-value tlb 'ptr))
(defmethod cffi:translate-from-foreign (pointer (type typelib-type))
  (make-typelib :ptr pointer))

(cffi:defcfun g-typelib-new-from-memory :pointer
  (memory :pointer)
  (len :uint)
  (gerror :pointer))

(defun typelib-new (source)
  (let ((buffer (g-malloc (length source))))
    (iter (for b in-vector source)
	  (for i from 0)
	  (setf (cffi:mem-aref buffer :uint8 i) b))
    (make-typelib :ptr (with-gerror err (g-typelib-new-from-memory buffer (length source) err)))))

(cffi:defcfun (typelib-free "g_typelib_free") :void
  (typelib typelib-type))

(defmacro with-typelib (var source &body body)
  `(let ((,var (typelib-new ,source)))
     (unwind-protect
	  (progn
	    ,@body)
       (typelib-free ,var))))

(defmacro with-typelibs ((&rest var-defs) &body body)
  `(let ,(iter (for vs on var-defs by #'cddr)
	       (collect `(,(first vs) (typelib-new ,(second vs)))))
     (unwind-protect
	  (progn
	    ,@body)
       ,@(iter (for vs on var-defs by #'cddr)
	       (collect `(typelib-free ,(first vs)))))))

(cffi:defcfun g-typelib-symbol :boolean
  (typelib typelib-type)
  (symbol-name :string)
  (symbol :pointer))

(defun typelib-symbol (typelib symbol-name)
  (cffi:with-foreign-object (s :pointer)
    (when (g-typelib-symbol typelib symbol-name s)
      (cffi:mem-ref s :pointer))))

(cffi:defcfun (typelib-namespace "g_typelib_get_namespace") :string
  (typelib typelib-type))
