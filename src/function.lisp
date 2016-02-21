(in-package :gir)

(defun method? (flags)
  (let (res)
    (dolist (flag flags res)
      (case flag
        (:is-constructor (return-from method? nil))
        (:is-method (setf res t))))))

(defun constructor? (flags)
  (member :is-constructor flags))

(defun class-function? (flags)
  (dolist (flag flags t)
    (case flag
      (:is-constructor (return nil))
      (:is-method (return nil))
      (:is-getter (return nil))
      (:is-setter (return nil))
      (:wraps-vfunc (return nil)))))

(defun any->pointer (value)
  (typecase value
    (struct (struct-this value))
    (object-instance (this-of value))
    (t value)))

(defun set-pointer (position value)
  (setf (cffi:mem-ref position :pointer) (any->pointer value)))

(defun pointer->giarg (giarg value)
  (set-pointer (cffi:foreign-slot-pointer giarg '(:union argument) 'v-pointer)
	       value))

(defmacro copy-slots ((copy-slots &optional src-slots dst-slots) (src dst)
		      &body body)
  (let ((src-copy-slots nil)
	(dst-copy-slots nil))
    (iter (for slot-spec :in copy-slots)
	  (etypecase slot-spec
	    (symbol (push (list slot-spec slot-spec) src-copy-slots)
		    (push (list (gensym) slot-spec) dst-copy-slots))
	    (list (ecase (length slot-spec)
		    (2 (push (list (first slot-spec) (second slot-spec))
			     src-copy-slots)
		       (push (list (gensym) (second slot-spec))
			     dst-copy-slots))
		    (3 (push (list (first slot-spec) (third slot-spec))
			     src-copy-slots)
		       (push (cdr slot-spec) dst-copy-slots))))))
    `(with-slots ,(append src-copy-slots src-slots)
	 ,src
       (with-slots ,(append dst-copy-slots dst-slots)
	   ,dst
	 ,@(iter (for src-slot :in src-copy-slots)
		 (for dst-slot :in dst-copy-slots)
		 (collect `(setf ,(first dst-slot) ,(first src-slot))))
	 ,@body))))

(defgeneric initialize-copy (obj copy))
(defgeneric find-object-with-class (obj class)
  (:method (obj class) (when (eq (class-of obj) class) obj)))
;; free-from/to-foreign-p is for pointed object
;; free-from-foreign-aggregated-p is for object itself (as part of the
;; container object)
(defgeneric free-from-foreign-aggregated-p (type)
  (:method (type) (declare (ignore type)) t))
(defgeneric free-from-foreign-p (type)
  (:method (type) (declare (ignore type))))
(defgeneric free-to-foreign-p (type)
  (:method (type) (declare (ignore type))))

(defun copy-instance (obj)
  (let ((copy (allocate-instance (class-of obj))))
    (initialize-copy obj copy)
    copy))

(defclass freeable-type ()
  ((free-from-foreign :initarg :free-from-foreign
		      :reader free-from-foreign-p
		      :initform nil :type boolean)
   (free-to-foreign :initarg :free-to-foreign
		    :reader free-to-foreign-p
		    :initform nil :type boolean)))

(defmethod initialize-copy ((obj freeable-type) (copy freeable-type))
  (copy-slots ((free-from-foreign free-to-foreign)) (obj copy)))

(defgeneric mem-size (type))
(defgeneric mem-set (pos value type))
(defgeneric mem-get (pos type))
(defgeneric mem-free (pos type)
  (:method (pos type) (declare (ignore pos type))))
(defgeneric mem-alloc (pos type))
(defgeneric alloc-foreign (type &key initial-value))
(defgeneric free-foreign (ptr type))

(defmethod alloc-foreign (type &key (initial-value nil initial-value-p))
  (let* ((size (mem-size type))
	 (pos (cffi:foreign-alloc :uint8 :count size)))
    (when initial-value-p
      (mem-set pos initial-value type))
    pos))

(defmethod free-foreign (ptr type)
  (unless (cffi:null-pointer-p ptr)
    (mem-free ptr type)
    (cffi:foreign-free ptr)))

(defclass pointer-type (freeable-type)
  ((pointed-type :initform nil :initarg :pointed-type :accessor pointed-type-of)))

(defmethod shared-initialize :after ((type pointer-type) slot-names
				     &key (free-to-foreign nil ftf-specified-p))
  (declare (ignore slot-names free-to-foreign))
  (with-slots ((ftf free-to-foreign) pointed-type)
      type
    (unless ftf-specified-p
      (setf ftf (if pointed-type t nil)))))

(defmethod mem-size ((type pointer-type))
  (declare (ignore type))
  (cffi:foreign-type-size :pointer))

(defmethod mem-set (pos value (type pointer-type))
  (with-slots (pointed-type)
      type
    (setf (cffi:mem-ref pos :pointer)
	  (if pointed-type
	      (alloc-foreign pointed-type :initial-value value)
	      value))))

(defmethod mem-set (pos (value null) (type pointer-type))
  (declare (ignore value))
  (setf (cffi:mem-ref pos :pointer) (cffi:null-pointer)))

(defmethod mem-alloc (pos (type pointer-type))
  (setf (cffi:mem-ref pos :pointer) (alloc-foreign (pointed-type-of type))))

(defmethod mem-get (pos (type pointer-type))
  (with-slots (pointed-type)
      type
    (let* ((ptr (cffi:mem-ref pos :pointer))
	   (value (if pointed-type
		      (mem-get ptr pointed-type)
		      ptr)))
      (if (and (free-from-foreign-p type)
	       (free-from-foreign-aggregated-p pointed-type))
	  (cffi:foreign-free ptr))
      value)))

(defmethod mem-free (pos (type pointer-type))
  (with-slots (pointed-type free-to-foreign)
      type
    (when (and pointed-type free-to-foreign)
      (let ((ptr (cffi:mem-ref pos :pointer)))
	(unless (cffi:null-pointer-p ptr)
	  (free-foreign ptr pointed-type))))))

(defmethod initialize-copy ((obj pointer-type) (copy pointer-type))
  (call-next-method)
  (copy-slots (nil (pointed-type) ((copy-pointed-type pointed-type))) (obj copy)
    (when pointed-type
      (setf copy-pointed-type (copy-instance pointed-type)))))

(defmethod find-object-with-class ((obj pointer-type) class)
  (if (eq (class-of obj) class)
      obj
      (find-object-with-class (pointed-type-of obj) class)))

(let ((void-pointer-type-cache (make-instance 'pointer-type)))
  (defun make-void-pointer-type ()
    void-pointer-type-cache))

(defclass c-array-type ()
  ((param-type :initform (error ":param-type must be supplied to make-instance")
               :initarg :param-type :reader param-type-of)
   (fixed-size :initform nil :initarg :fixed-size)
   (zero-terminated? :initform nil :initarg :zero-terminated?)
   (length :initform nil :initarg :length :accessor length-of)))

(defmethod free-from-foreign-aggregated-p ((c-array-type c-array-type))
  (free-from-foreign-aggregated-p (param-type-of c-array-type)))

(defun c-array-length (c-array-type)
  (with-slots (param-type fixed-size zero-terminated? length)
      c-array-type
    (if fixed-size
	(if (and length (< length fixed-size))
	    length
	    fixed-size)
	(if length
	    (+ length (if zero-terminated? 1 0))))))

(defmethod mem-size ((c-array-type c-array-type))
  (with-slots (param-type)
      c-array-type
    (* (mem-size param-type)
       (c-array-length c-array-type))))

(defmethod mem-set (array value (c-array-type c-array-type))
  (with-slots (param-type fixed-size zero-terminated?)
      c-array-type
    (let ((length (c-array-length c-array-type))
	  (param-size (mem-size param-type)))
      (loop
	 :for pos = array :then (cffi:inc-pointer pos param-size)
	 :for i :below (if zero-terminated? (1- length) length)
	 :for element :in value
	 :do (mem-set pos element param-type)
	 :finally (if zero-terminated?
		      (zero-memory pos param-size))))))

(defun map-c-array (func array c-array-type)
  (with-slots (param-type fixed-size zero-terminated?)
      c-array-type
    (let ((param-size (mem-size param-type))
	  (length (c-array-length c-array-type)))
      (loop
	 :for i :upfrom 0
	 :for pos = array :then (cffi:inc-pointer pos param-size)
	 :until (and (or (not zero-terminated?) length)
		     (>= i length))
	 :until (and zero-terminated? (zero? pos param-size))
	 :collect (funcall func pos)))))

(defmethod mem-free (array (c-array-type c-array-type))
  (let ((param-type (param-type-of c-array-type)))
    (map-c-array (lambda (pos) (mem-free pos param-type)) array c-array-type)))

(defmethod mem-get (array (c-array-type c-array-type))
  (let ((param-type (param-type-of c-array-type)))
    (map-c-array (lambda (pos) (mem-get pos param-type)) array c-array-type)))

(defmethod initialize-copy ((obj c-array-type) (copy c-array-type))
  (copy-slots
      ((fixed-size zero-terminated? length) (param-type) ((copy-param-type param-type)))
      (obj copy)
    (setf copy-param-type (copy-instance param-type))))

(defmethod find-object-with-class ((obj c-array-type) class)
  (if (eq (class-of obj) class)
      obj
      (find-object-with-class (param-type-of obj) class)))

(defclass object/struct-pointer-type (pointer-type)
  ())

(defmethod mem-set (pos value (o/s-t object/struct-pointer-type))
  (let ((pointer (etypecase value
		   (struct (struct-this value))
		   (object-instance (this-of value))
		   (cffi:foreign-pointer value))))
    (setf (cffi:mem-ref pos :pointer) pointer)))

(let ((o/s-p-t-cache (make-instance 'object/struct-pointer-type)))
  (defun make-object/struct-pointer-type ()
    o/s-p-t-cache))

(defclass interface-type ()
  ((namespace :initarg :namespace)
   (name :initarg :name)
   (gir-class :initform nil :initarg :gir-class)))

(defmethod gir-class-of ((interface-type interface-type))
  (with-slots (gir-class)
      interface-type
    (or gir-class
	(with-slots (namespace name)
	    interface-type
	  (setf gir-class (find-build-interface-for-name namespace name))))))

(defmethod initialize-copy ((obj interface-type) (copy interface-type))
  (copy-slots ((namespace name gir-class)) (obj copy)))

(defclass object-pointer-type (pointer-type interface-type)
  ())

(defmethod mem-set (pos value (object-pointer-type object-pointer-type))
  (setf (cffi:mem-ref pos :pointer) (this-of value)))

(defmethod mem-set (pos (value null) (object-pointer-type object-pointer-type))
  (declare (ignore value))
  (setf (cffi:mem-ref pos :pointer) (cffi:null-pointer)))

(defmethod mem-free (pos (type object-pointer-type))
  (declare (ignore pos type)))

(defmethod mem-get (pos (object-pointer-type object-pointer-type))
  (with-accessors ((free-from-foreign free-from-foreign-p) (gir-class gir-class-of))
      object-pointer-type
    (let ((ptr (cffi:mem-ref pos :pointer)))
     (if (cffi:null-pointer-p ptr)
	 nil
	 (if-let ((obj (build-object-ptr gir-class ptr)))
	   (if free-from-foreign
	       (object-setup-gc obj :everything))
	   obj)))))

(defmethod initialize-copy ((obj object-pointer-type) (copy object-pointer-type))
  (call-next-method))

(let ((object-pointer-type-cache (make-hash-table :test #'equal)))
  (defun make-object-pointer-type (namespace name free-from-foreign)
    (ensure-gethash (list namespace name free-from-foreign) object-pointer-type-cache
		    (make-instance 'object-pointer-type :namespace namespace :name name
				   :free-from-foreign free-from-foreign))))

(defclass struct-pointer-type (pointer-type interface-type)
  ())

(defmethod shared-initialize :after ((type struct-pointer-type) slot-names
				     &key namespace name)
  (setf (pointed-type-of type)
	(make-instance 'struct-type :namespace namespace :name name)))

(defmethod mem-set (pos value (struct-pointer-type struct-pointer-type))
  (declare (ignore struct-pointer-type))
  (setf (cffi:mem-ref pos :pointer)
	(if (typep value 'struct)
	    (struct-this value)
	    value)))

(defmethod mem-set (pos (value null) (struct-pointer-type struct-pointer-type))
  (declare (ignore struct-pointer-type))
  (setf (cffi:mem-ref pos :pointer) (cffi:null-pointer)))

(defmethod mem-get (pos (struct-pointer-type struct-pointer-type))
  (with-accessors ((gir-class gir-class-of))
      struct-pointer-type
    (let ((ptr (cffi:mem-ref pos :pointer)))
     (if (cffi:null-pointer-p ptr)
	 nil
	 (build-struct-ptr gir-class ptr)))))

(defmethod mem-free (pos (type struct-pointer-type))
  (declare (ignore pos type)))

(defmethod initialize-copy ((obj struct-pointer-type) (copy struct-pointer-type))
  (call-next-method))

(let ((struct-pointer-type-cache (make-hash-table :test #'equal)))
  (defun make-struct-pointer-type (namespace name)
    (ensure-gethash (list namespace name) struct-pointer-type-cache
		    (make-instance 'struct-pointer-type :namespace namespace :name name))))

(defclass struct-type (interface-type)
  ())

(defmethod free-from-foreign-aggregated-p ((struct-type struct-type))
  (declare (ignore struct-type)))

(defmethod mem-size ((struct-type struct-type))
  (with-accessors ((gir-class gir-class-of))
      struct-type
    (struct-info-get-size (struct-class-info gir-class))))

(defmethod mem-set (pos value (struct-type struct-type))
  (copy-memory pos (struct-this value) (mem-size struct-type)))

(defmethod mem-get (pos (struct-type struct-type))
  (with-accessors ((gir-class gir-class-of))
      struct-type
    (build-struct-ptr gir-class pos)))

(defmethod initialize-copy ((obj struct-type) (copy struct-type))
  (call-next-method))

(let ((struct-type-cache (make-hash-table :test #'equal)))
  (defun make-struct-type (namespace name)
    (ensure-gethash (list namespace name) struct-type-cache
		    (make-instance 'struct-type :namespace namespace :name name))))

(defclass union-type ()
  ((size :initarg :size)))

(defmethod free-from-foreign-aggregated-p ((union-type union-type))
  (declare (ignore union-type)))

(defmethod mem-size ((union-type union-type))
  (slot-value union-type 'size))

(defmethod mem-set (pos value (union-type union-type))
  (copy-memory pos value (mem-size union-type)))

(defmethod mem-get (pos (union-type union-type))
  (declare (ignore union-type))
  pos)

(defmethod initialize-copy ((obj union-type) (copy union-type))
  (copy-slots ((size)) (obj copy)))

(let ((union-type-cache (make-hash-table :test #'equal)))
  (defun make-union-type (namespace name)
    (ensure-gethash (list namespace name) union-type-cache
		    (make-instance 'union-type :namespace namespace :name name))))

(defclass string-pointer-type (pointer-type)
  ()
  (:default-initargs :free-to-foreign t))

(defmethod mem-set (pos value (string-pointer-type string-pointer-type))
  (declare (ignore string-pointer-type))
  (setf (cffi:mem-ref pos :pointer) (cffi:foreign-string-alloc value)))

(defmethod mem-get (pos (string-pointer-type string-pointer-type))
  (let* ((ptr (cffi:mem-ref pos :pointer))
	 (str (cffi:foreign-string-to-lisp ptr)))
    (when (free-from-foreign-p string-pointer-type)
      (cffi:foreign-string-free ptr))
    str))

(defmethod mem-free (pos (string-pointer-type string-pointer-type))
  (let ((ptr (cffi:mem-ref pos :pointer)))
    (when (and (free-to-foreign-p string-pointer-type) (not (cffi:null-pointer-p ptr)))
      (cffi:foreign-string-free ptr))))

(defmethod initialize-copy ((obj string-pointer-type) (copy string-pointer-type))
  (call-next-method))

(let (string-pointer-type-free-from-foreign
      string-pointer-type-dont-free-from-foreign)
  (defun make-string-pointer-type (free-from-foreign)
    (if free-from-foreign
	(or string-pointer-type-free-from-foreign
	    (setf string-pointer-type-free-from-foreign
		  (make-instance 'string-pointer-type :free-from-foreign t)))
	(or string-pointer-type-dont-free-from-foreign
	    (setf string-pointer-type-dont-free-from-foreign
		  (make-instance 'string-pointer-type :free-from-foreign nil))))))

(defclass void-type ()
  ())

(defmethod mem-size ((void-type void-type))
  (cffi:foreign-type-size :pointer))

(defmethod mem-set (pos value (void-type void-type))
  (declare (ignore value void-type))
  (setf (cffi:mem-ref pos :pointer) (cffi:null-pointer)))

(defmethod mem-get (pos (void-type void-type))
  (declare (ignore pos void-type)))

(defmethod initialize-copy ((obj void-type) (copy void-type))
  (declare (ignore obj copy)))

(let ((void-type-cache (make-instance 'void-type)))
  (defun make-void-type ()
    void-type-cache))

(defclass builtin-type ()
  ((cffi-type :initarg :cffi-type :reader cffi-type-of)))

(defmethod mem-size ((builtin-type builtin-type))
  (cffi:foreign-type-size (cffi-type-of builtin-type)))

(defmethod mem-set (pos value (builtin-type builtin-type))
  (setf (cffi:mem-ref pos (cffi-type-of builtin-type)) value))

(defmethod mem-get (pos (builtin-type builtin-type))
  (cffi:mem-ref pos (cffi-type-of builtin-type)))

(defmethod initialize-copy ((obj builtin-type) (copy builtin-type))
  (copy-slots ((cffi-type)) (obj copy)))

(defclass argument-type ()
  ((contained-type :initarg :contained-type :reader contained-type-of)
   (field :initarg :field)))

(defmethod mem-size ((argument-type argument-type))
  (cffi:foreign-type-size '(:union argument)))

(defmethod mem-set (pos value (argument-type argument-type))
  (with-slots (contained-type field)
      argument-type
    (mem-set (cffi:foreign-slot-pointer pos '(:union argument) field) value contained-type)))

(defmethod mem-alloc (pos (argument-type argument-type))
  (with-slots (contained-type field)
      argument-type
    (mem-alloc (cffi:foreign-slot-pointer pos '(:union argument) field) contained-type)))

(defmethod mem-get (pos (argument-type argument-type))
  (with-slots (contained-type field)
      argument-type
    (mem-get (cffi:foreign-slot-pointer pos '(:union argument) field) contained-type)))

(defmethod mem-free (pos (argument-type argument-type))
  (with-slots (contained-type field)
      argument-type
    (mem-free (cffi:foreign-slot-pointer pos '(:union argument) field) contained-type)))

(defmethod initialize-copy ((obj argument-type) (copy argument-type))
  (copy-slots ((field) (contained-type) ((copy-contained-type contained-type))) (obj copy)
    (when contained-type
      (setf copy-contained-type (copy-instance contained-type)))))

(defmethod find-object-with-class ((obj argument-type) class)
  (if (eq (class-of obj) class)
      obj
      (find-object-with-class (contained-type-of obj) class)))

(defun parse-array-type-info (type-info transfer)
  (if (eq (type-info-get-array-type type-info) :c)
      (let* ((param-type-info (type-info-get-param-type type-info 0))
	     (param-transfer (if (eq transfer :everything)
				 :everything
				 :nothing))
	     (param-type (parse-type-info param-type-info param-transfer))
	     (zero-terminated? (type-info-is-zero-terminated type-info))
	     (fixed-size (let ((size (type-info-get-array-fixed-size type-info)))
			   (if (/= -1 size) size)))
	     (array-type (make-instance 'c-array-type :param-type param-type :fixed-size fixed-size
					:zero-terminated? zero-terminated?)))
	(make-instance 'pointer-type :pointed-type array-type
		       :free-from-foreign (not (eq transfer :nothing))))
      (make-void-pointer-type)))

(defun make-interface-pointer-type (interface-info transfer)
  (let ((namespace (info-get-namespace interface-info))
	(name (info-get-name interface-info))
	(free-from-foreign (eq transfer :everything)))
    (typecase interface-info
      (object-info (make-object-pointer-type namespace name free-from-foreign))
      (struct-info (make-struct-pointer-type namespace name))
      (t (make-void-pointer-type)))))

(defun parse-interface-pointer-type-info (type-info transfer)
  (make-interface-pointer-type (type-info-get-interface type-info) transfer))

(defun parse-interface-type-info (type-info)
  (let* ((interface-info (type-info-get-interface type-info))
	 (namespace (info-get-namespace interface-info))
	 (name (info-get-name interface-info)))
    (typecase interface-info
      (struct-info (make-struct-type namespace name))
      (union-info (make-union-type namespace name))
      (t (find-parse-general-type-info :uint)))))

(defun parse-general-type-info (tag)
  (multiple-value-bind (cffi-type field)
      (case tag
	(:boolean (values :boolean 'v-boolean))
	(:int8 (values :int8 'v-int8))
	(:uint8 (values :uint8 'v-uint8))
	(:int16 (values :uint16 'v-int16))
	(:uint16 (values :uint16 'v-uint16))
	(:int32 (values :int32 'v-int32))
	(:uint32 (values :uint32 'v-uint32))
	(:int64 (values :int64 'v-int64))
	(:uint64 (values :uint64 'v-uint64))
	(:short (values :short 'v-short))
	(:ushort (values :ushort 'v-ushort))
	(:int (values :int 'v-int))
	(:uint (values :uint 'v-uint))
	(:long (values :long 'v-long))
	(:ulong (values :ulong 'v-ulong))
	(:ssize (values :long 'v-long))
	(:size (values :ulong 'v-ulong))
	(:float (values :float 'v-float))
	(:double (values :double 'v-double))
	(:time-t (values :long 'v-long))
	(:gtype (values :ulong 'v-ulong))
	(:unichar (values :int32 'v-uint32))
	(t (values :pointer 'v-pointer)))
    (list (make-instance 'builtin-type :cffi-type cffi-type) field)))

(let ((general-type-info-cache (make-hash-table)))
  (defun find-parse-general-type-info (tag)
    (values-list (ensure-gethash tag general-type-info-cache
				 (parse-general-type-info tag)))))

(defun parse-type-info (type-info transfer &key force-pointer)
  (let ((pointerp (or force-pointer (type-info-is-pointer type-info)))
	(tag (type-info-get-tag type-info)))
    (if pointerp
	(values (case tag
		  (:array (parse-array-type-info type-info transfer))
		  (:interface (parse-interface-pointer-type-info type-info transfer))
		  ((:utf8 :filename) (make-string-pointer-type (eq transfer :everything)))
		  (t :pointer))
		'v-pointer)
	(case tag
	  (:interface (values (parse-interface-type-info type-info) 'v-uint))
	  (:void (values (make-void-type) 'v-pointer))
	  ((:array :utf8 :filename)
	   (error "array, utf8, filename must be pointer"))
	  (t (find-parse-general-type-info tag))))))

(defun build-argument-type (type-info transfer &key force-pointer)
  (multiple-value-bind (foreign-type field)
      (parse-type-info type-info transfer :force-pointer force-pointer)
    (make-instance 'argument-type :contained-type foreign-type :field field)))

(defun copy-find-set-c-array-type-length (type length)
  (let* ((copy-type (copy-instance type))
	 (c-array-type (find-object-with-class copy-type (find-class 'c-array-type))))
    (setf (length-of c-array-type) length)
    copy-type))

(defun get-array-length (type)
  (let ((length (type-info-get-array-length type)))
    (if (eql length -1) nil length)))

(defclass arg-data ()
  ((type)
   (is-array-type)
   (direction :reader direction-of)
   (for-array-length-p :initform nil :accessor for-array-length-p-of)
   (array-length :reader array-length-of)
   (caller-allocates)
   (transfer)))

(defmethod shared-initialize :after ((arg-data arg-data)
				     slot-names &key arg-info)
  (with-slots (type is-array-type direction for-array-length
		    array-length caller-allocates)
      arg-data
    (case arg-info
      (:object-argument
       (setf type (make-object/struct-pointer-type)
	     caller-allocates nil
	     direction :in
	     is-array-type nil
	     array-length nil))
      (otherwise
       (let ((type-info (arg-info-get-type arg-info))
	     (transfer (arg-info-get-ownership-transfer arg-info)))
	 (setf caller-allocates (arg-info-is-caller-allocates arg-info)
	       type (build-argument-type type-info transfer
					 :force-pointer caller-allocates)
	       direction (arg-info-get-direction arg-info)
	       is-array-type
	       (find-object-with-class type (find-class 'c-array-type))
	       array-length (get-array-length type-info)))))))

(let ((o-a-d-cache (make-instance 'arg-data
				  :arg-info :object-argument)))
  (defun object-arg-data ()
    o-a-d-cache))

(defun make-args-data (info)
  ;; if construct + in-arg
  (let* ((n-args (g-callable-info-get-n-args info))
	 (args-data
	  (when (method? (function-info-get-flags info))
	    (list (object-arg-data))))
	 (in-count (length args-data))
	 (out-count 0)
	 (in-array-length-count 0))
    (dotimes (i n-args)
      (let* ((arg (g-callable-info-get-arg info i))
             (direction (arg-info-get-direction arg))
	     (arg-data (make-instance 'arg-data :arg-info arg)))
	(ecase direction
	  (:in (incf in-count))
	  (:in-out (incf in-count) (incf out-count))
	  (:out (incf out-count)))
	(push arg-data args-data)))
    (setf args-data (nreverse args-data))
    (loop
       :for arg-data :in args-data
       :for array-length = (array-length-of arg-data)
       :when array-length
       :do (setf (for-array-length-p-of
		  (nth array-length args-data)) t))
    (loop
       :for arg-data :in args-data
       :when (and (not (eq (direction-of arg-data) :out))
		  (for-array-length-p-of arg-data))
       :do (incf in-array-length-count))
    (values args-data in-count out-count in-array-length-count)))

(defclass arg ()
  ((data :initarg :data)
   (giarg)
   (value)
   (length-arg)))

(defmacro incf-giargs (giargs)
  `(setf ,giargs (cffi:mem-aptr ,giargs '(:union argument) 1)))

(defun make-arg (data inp outp voutp)
  (let ((arg (make-instance 'arg :data data)))
    (with-slots (giarg value)
	arg
      (with-slots (type caller-allocates direction)
	  data
	(ecase direction
	  (:in
	   (setf giarg inp)
	   (incf-giargs inp))
	  (:in-out
	   (setf giarg voutp)
	   (pointer->giarg inp voutp)
	   (pointer->giarg outp voutp)
	   (incf-giargs inp)
	   (incf-giargs outp)
	   (incf-giargs voutp))
	  (:out
	   (if caller-allocates
	       (progn
		 (setf giarg outp)
		 (mem-alloc giarg type))
	       (progn
		 (setf giarg voutp)
		 (pointer->giarg outp voutp)))
	   (incf-giargs outp)
	   (incf-giargs voutp)))))
    (values arg inp outp voutp)))

(defun make-args (args-data giargs-in giargs-out values-out)
  (iter	(for data :in args-data)
	(for (values arg inp outp voutp)
	     :first (make-arg data giargs-in giargs-out values-out)
	     :then (make-arg data inp outp voutp))
	(collect arg)))

(defun arg-setup-length (arg args)
  (with-slots (data length-arg)
      arg
    (with-slots (array-length)
	data
      (setf length-arg
	    (if array-length (nth array-length args))))))

(defun in-arg-setup (arg value)
  (with-slots (data giarg length-arg (arg-value value))
      arg
    (with-slots (type is-array-type)
	data
      (setf arg-value value)
      (let ((real-type (if is-array-type
			   (copy-find-set-c-array-type-length
			    type (length value))
			   type)))
	(mem-set giarg value real-type)
	(when length-arg
	  (in-arg-setup length-arg (length value)))))))

(defun out-arg->value (arg)
  (with-slots (data giarg length-arg)
      arg
    (with-slots (type)
	data
      (let ((real-type
	     (if length-arg
		 (copy-find-set-c-array-type-length type
						    (out-arg->value length-arg))
		 type)))
	(mem-get giarg real-type)))))

(defun in-arg-clear (arg)
  (with-slots (data giarg length-arg (arg-value value))
      arg
    (with-slots (type is-array-type direction)
	data
      (when (eq direction :in)
	(let ((real-type
	       (if is-array-type
		   (copy-find-set-c-array-type-length type (length arg-value))
		   type)))
	  (mem-free giarg real-type))))))

(defun in/out-args (args)
  (iter	(for arg :in args)
	(with-slots (data)
	    arg
	  (with-slots (direction for-array-length-p)
	      data
	    (when (and (eq direction :in) (not for-array-length-p))
	      (collect arg :into pure-in-args))
	    (when (and (or (eq direction :in) (eq direction :in-out))
		       (not for-array-length-p))
	      (collect arg :into in-args))
	    (when (and (or (eq direction :out) (eq direction :in-out))
		       (not for-array-length-p))
	      (collect arg :into out-args))))
	(finally (return (values pure-in-args in-args out-args)))))

(defun check-args (args in-count name)
  (assert (= in-count (length args))
          (args)
          "Should be ~a arguments in function ~a"
	  in-count name))

(defclass return-data ()
  ((type)
   (array-length :reader array-length-of)))

(defmethod shared-initialize :after ((return-data return-data)
				     slot-names &key func-info return-interface)
  (with-slots (type array-length)
      return-data
    (let ((type-info (callable-info-get-return-type func-info))
	  (transfer (callable-info-get-caller-owns func-info)))
      (setf type
	    (if return-interface
		(let ((intf-ptr-type (make-interface-pointer-type return-interface :everything)))
		  (make-instance 'argument-type :contained-type intf-ptr-type :field 'v-pointer))
		(build-argument-type type-info transfer))
	    array-length (get-array-length type-info)))))

(defclass return-value ()
  ((data :initarg :data)
   (giarg :initarg :giarg)
   (length-arg)))

(defun return-value-setup-length (ret-val args)
  (with-slots (data length-arg)
      ret-val
    (with-slots (array-length)
	data
      (setf length-arg
	    (if array-length (nth array-length args))))))

(defun return-value->value (ret-val)
  (with-slots (data giarg length-arg)
      ret-val
    (with-slots (type)
	data
      (let ((real-type
	     (if length-arg
		 (copy-find-set-c-array-type-length type
						    (out-arg->value length-arg))
		 type)))
	(mem-get giarg real-type)))))

(defun make-out (ret-val out-args)
  (cons
   (return-value->value ret-val)
   (mapcar #'out-arg->value out-args)))

(cffi:defcfun g-function-info-invoke :boolean
  (info info-ffi)
  (in-args :pointer) (n-in-args :int)
  (out-args :pointer) (n-out-args :int)
  (ret :pointer) (g-error :pointer))

(defun build-function (info &key return-interface)
  (multiple-value-bind (args-data in-count out-count in-array-length-count)
      (make-args-data info)
    (let* ((name (info-get-name info))
	   (ret-data (make-instance 'return-data :func-info info
				    :return-interface return-interface)))
      (lambda (&rest args-in)
	(check-args args-in (- in-count in-array-length-count) name)
	(values-list
	 (cffi:with-foreign-objects
	     ((giargs-in '(:union argument) in-count)
	      (giargs-out '(:union argument) out-count)
	      (values-out '(:union argument) out-count)
	      (giarg-res '(:union argument)))
	   (let ((args (make-args args-data
				  giargs-in giargs-out values-out))
		 (ret-val (make-instance 'return-value :data ret-data
					 :giarg giarg-res)))
	     (iter (for arg :in args)
		   (arg-setup-length arg args))
	     (return-value-setup-length ret-val args)
	     (multiple-value-bind (pure-in-args in-args out-args)
		 (in/out-args args)
	       (mapc #'in-arg-setup in-args args-in)
	       (unwind-protect
		    (with-gerror g-error
		      (g-function-info-invoke info
					      giargs-in in-count
					      giargs-out out-count
					      giarg-res g-error)
		      (make-out ret-val out-args))
		 (mapc #'in-arg-clear pure-in-args))))))))))

(defmethod build-interface ((info function-info))
  (build-function info))

