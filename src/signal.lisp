(in-package :gir)

(declaim (optimize (debug 3) (speed 0)))

(cffi:defcfun g-closure-add-finalize-notifier :void
  (closure :pointer) (data :pointer) (func :pointer))

(cffi:defcfun g-closure-new-simple :pointer (sizeof :int) (data :pointer))

(cffi:defcfun g-closure-set-marshal :void (closure :pointer) (marshal :pointer))

(cffi:defcfun g-signal-connect-closure :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure :pointer)
  (after :boolean))

;; Define the closure struct to compute its size
(cffi:defcstruct g-closure
    (flags :unsigned-int)
  (marshal :pointer)
  (data :pointer)
  (notifiers :pointer))

(defvar *objects* (make-hash-table))

(cffi:defcallback marshal :void ((closure :pointer)
                                 (return :pointer)
                                 (n-values :int)
                                 (params :pointer)
                                 (hint :pointer)
                                 (data :pointer))
  (declare (ignore hint data))
  (let ((lisp-func (gethash (cffi:pointer-address closure) *objects*))
        (lisp-params 
          (loop
            :for i :below n-values
            :collect 
            (let* ((gvalue (cffi:mem-aptr
			    params
			    '(:struct g-value-struct)
			    i))
		   (val (gvalue->lisp/free gvalue (gvalue-gtype gvalue)
					   :no-free t)))
	      (if (typep val 'object-instance)
		  (object-setup-gc val :nothing)
		  val)))))
    (let ((res (apply lisp-func lisp-params)))
      (unless (cffi:null-pointer-p return)
        (set-value! return (gvalue-gtype return) res)))))

(cffi:defcallback free-closure :void ((data :pointer) (closure :pointer))
  (declare (ignore data))
  (when (not (cffi:null-pointer-p closure))
    (remhash (cffi:pointer-address closure) *objects*)))

(defun make-closure (func)
  (let* ((g-closure-size (cffi:foreign-type-size '(:struct g-closure)))
         (closure-ptr (g-closure-new-simple
                       g-closure-size (cffi:null-pointer)))) ;; sizeof(GClosure) = 16
    (setf (gethash (cffi:pointer-address closure-ptr) *objects*) func)
    (g-closure-set-marshal closure-ptr (cffi:callback marshal))
    (g-closure-add-finalize-notifier closure-ptr
                                     (cffi:null-pointer)
                                     (cffi:callback free-closure))
    closure-ptr))

(defun c-func (value)
  (labels ((to-ptr (str)
             (declare (type string str))
             (cffi:foreign-symbol-pointer (substitute #\_ #\- str))))
    (etypecase value
      (string (to-ptr value))
      (keyword (to-ptr (string-downcase value)))
      (cffi:foreign-pointer value)
      (null (cffi:null-pointer)))))

(defun connect (g-object signal c-handler &key after swapped)
  (let* ((object-ptr (if (typep g-object 'object-instance)
                         (this-of g-object)
                         g-object))
         (str-signal (string-downcase signal))
         (c-handler (cond 
                      ((and (symbolp c-handler) (fboundp c-handler))
                       (symbol-function c-handler))
                      ((functionp c-handler) c-handler)
                      (t (c-func c-handler))))
         (flags (+ (if after 1 0) (if swapped 2 0)))
         (handler-id
           (typecase c-handler
             (function (g-signal-connect-closure 
			object-ptr str-signal
			(make-closure c-handler)
			after))
             (t (g-signal-connect-data object-ptr
                                       str-signal 
                                       c-handler
                                       (cffi:null-pointer)
                                       (cffi:null-pointer) 
                                       flags)))))
    handler-id))

(cffi:defcfun g-signal-handler-disconnect :void (instance :pointer) (id :ulong))
(defun disconnect (g-object id)
  (let* ((object-ptr (if (typep g-object 'object-instance)
                         (this-of g-object)
                         g-object)))
    (g-signal-handler-disconnect object-ptr id)))
