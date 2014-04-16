(in-package :gir)

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

(defun closures (info)
  (let* ((call (lambda (name args)
                 (let ((function-info (find-ffi-method info (c-name name))))
                   (if function-info
                       (apply (build-function function-info) args)
                       (error "Bad FFI method name ~a" name)))))
         (fields-dict
          (loop :for i :below (g-object-info-get-n-fields info)
             :collect
             (let ((field-info (g-object-info-get-field info i)))
               (cons (info-get-name field-info) field-info))))
         (find-field (lambda (name)
                       (cdr (or (assoc (c-name name) fields-dict :test #'string=)
                                (error "Bad FFI field name ~a" name)))))
         (closure (lambda (this)
                    (let ((signals (list nil)))
                      (lambda (name)
                        (case name
                          (:this this)
                          (:signals signals)
                          (:field
			   (lambda (name)
                             (gir.field:get this (funcall find-field name))))
                          (:set-field!
			   (lambda (name value)
                             (gir.field:set this 
                                            (funcall find-field name) 
                                            value)))
                          (:set-properties!
			   (lambda (&rest args)
			     (set-properties! this args)))
                          (:properties
			   (lambda (&rest args)
			     (get-properties this args)))
                          (t
			   (lambda (&rest args)
			     (funcall call name (cons this args))))))))))
    closure))

;; wiil be defined later
(defun get-properties (ptr args)
  (declare (ignore ptr args)))
(defun set-properties! (ptr args)
  (declare (ignore ptr args)))

(cffi:defcfun g-object-ref-sink :pointer (obj :pointer))
(cffi:defcfun g-object-unref :void (obj :pointer))

(defun object-ref-sink (obj)
  (let* ((res (g-object-ref-sink obj))
         (a (cffi:pointer-address res)))
    (tg:finalize res (lambda () (g-object-unref (cffi:make-pointer a))))
    res))

(defun build-object (info)
  (let ((closure (closures info)))
    (lambda (name)
      (if (cffi:pointerp name)
	  (funcall closure name)
	  (let* ((function-info (find-ffi-method info (c-name name)))
		 (flags))
	    (if function-info
		(setf flags (function-info-get-flags function-info))
		(error "Bad FFI constructor/function name ~a" name))
	    (cond
	      ((constructor? flags)
	       (lambda (&rest args)
		 (let ((this (apply (build-function function-info
						    :return-raw-pointer t) args)))
		   (funcall closure (object-ref-sink this)))))
	      ((class-function? flags)
	       (build-function function-info))
	      (t
	       (error "~a is not constructor or class function" name))))))))

(defun build-object-ptr (info ptr)
  (let ((closure (closures info)))
    (funcall closure ptr)))

(defun gobject (gtype ptr)
  (let ((info (repository-find-by-gtype nil gtype)))
    (if (and info (eq (info-get-type info) :object))
        (build-object-ptr info ptr)
        (error "gtype ~a not found in GI. Found ~a" 
               gtype (info-get-type info)))))

(cffi:define-foreign-type pobject ()
  ()
  (:documentation "pointer to GObject")
  (:actual-type :pointer)
  (:simple-parser pobject))

(defmethod cffi:translate-to-foreign (object (type pobject))
  (nget object :this))

(defmethod cffi:translate-from-foreign (pointer (type pobject))
  (gobject (gtype pointer) pointer))
