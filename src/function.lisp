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

(defstruct
    (converter
      (:constructor make-converter (size set get free gc)))
  size set get free gc)

(defstruct 
    (translator 
      (:constructor make-translator (>giarg >value free gc)))
  >giarg >value free gc)

(defun any->pointer (value)
  (typecase value
    (struct (struct-this value))
    (object (object-this value))
    (t value)))

(defun set-pointer (position value)
  (setf (cffi:mem-ref position :pointer) (any->pointer value)))

(defun get-pointer (position &optional length)
  (declare (ignore length))
  (cffi:mem-ref position :pointer))

(defun pointer->giarg (giarg value)
  (set-pointer (cffi:foreign-slot-pointer giarg '(:union argument) 'v-pointer)
	       value))

(defun giarg->pointer (giarg)
  (get-pointer (cffi:foreign-slot-pointer giarg '(:union argument) 'v-pointer)))

(defun dont-free (giarg &optional length)
  (declare (ignore giarg length)))

(defun dont-gc (value transfer)
  (declare (ignore value transfer)))

(defun proc-dont-gc (value)
  (declare (ignore value)))

(defun build-array-converter (type)
  (let* ((param-type (type-info-get-param-type type 0))
	 (param-converter (build-converter param-type))
	 (param-size (converter-size param-converter))
	 (param-set (converter-set param-converter))
	 (param-get (converter-get param-converter))
	 (param-free (converter-free param-converter))
	 (zero-terminated? (type-info-is-zero-terminated type))
	 (fixed-size (let ((size (type-info-get-array-fixed-size type)))
		       (if (/= -1 size) size)))
	 (array-type (type-info-get-array-type type))
         (set
	  (case array-type
	    (:c
	     (lambda (position value)
	       (let* ((length (if fixed-size fixed-size
				  (+ (length value)
				     (if zero-terminated? 1 0))))
		      (array (cffi:foreign-alloc
			      :int8 :count (* length param-size))))
		 (loop
		    :for pos = array :then (cffi:inc-pointer pos param-size)
		    :for i :below (if zero-terminated? (1- length) length)
		    :for element :in value
		    :do (funcall param-set pos element)
		    :finally (if zero-terminated?
				 (zero-memory pos param-size)))
		 (set-pointer position array))))
	    (t #'set-pointer)))
         (get
	  (case array-type
	    (:c
	     (lambda (position &optional length)
	       (if (and fixed-size (or (null length) (> length fixed-size)))
		   (setf length fixed-size))
	       (loop
		  :for i :upfrom 0
		  :for pos = (get-pointer position)
		  :then (cffi:inc-pointer pos param-size)
		  :until (and (or (not zero-terminated?) length) (>= i length))
		  :until (and zero-terminated? (zero? pos param-size))
		  :collect (funcall param-get pos))))
	    (t #'get-pointer)))
         (free
	  (case array-type
	    (:c
	     (lambda (position &optional length)
	       (if (and fixed-size (or (null length) (> length fixed-size)))
		   (setf length fixed-size))
	       (let ((array (get-pointer position)))
		 (loop
		    :for i :upfrom 0
		    :for pos = array
		    :then (cffi:inc-pointer pos param-size)
		    :until (and (or (not zero-terminated?) length)
				(>= i length))
		    :until (and zero-terminated? (zero? pos param-size))
		    :do (if (funcall param-free pos) (return t))
		    :finally (cffi:foreign-free array)))))
	    (t #'dont-free)))
	 (gc
	  (lambda (value transfer)
	    (let ((param-transfer (if (eq transfer :container) :nothing
				      transfer)))
	      (dolist (item value)
		(funcall (converter-gc param-converter)
			 item param-transfer))))))
    (make-converter nil set get free gc)))

(defun build-interface-converter (type)
  (let* ((pointer? (type-info-is-pointer type))
	 (interface (type-info-get-interface type))
	 (size (if (not pointer?)
		   (typecase interface
		     (struct-info
		      (struct-info-get-size interface))
		     (union-info
		      (union-info-get-size interface))
		     (t
		      (cffi:foreign-type-size :uint)))
		   (cffi:foreign-type-size :pointer)))
         (set
	  (if pointer?
	      (lambda (position value)
		(set-pointer position (if value value (cffi:null-pointer))))
	      (typecase interface
		((or union-info struct-info)
		 (lambda (position value)
		   (copy-memory position (any->pointer value) size)))
		(t
		 (lambda (position value)
		   (setf (cffi:mem-ref position :uint) value))))))
         (get
           (if pointer?
	       (typecase interface
		 (struct-info
		  (let ((class (find-build-interface interface)))
		    (lambda (position)
		      (let ((this (get-pointer position)))
			(if (cffi:null-pointer-p this) nil
			    (build-struct-ptr class this))))))
		 (object-info
		  (let ((class (find-build-interface interface)))
		    (lambda (position)
		      (let ((this (get-pointer position)))
			(if (cffi:null-pointer-p this) nil
			    (build-object-ptr class this))))))
		 (t #'get-pointer))
	       (typecase interface
		 (struct-info
		  (let ((struct-class (find-build-interface interface)))
		    (lambda (position)
		      (build-struct-ptr struct-class position))))
		 (union-info
		  (lambda (position) position))
		 (t
		  (lambda (position)
		    (cffi:mem-ref position :uint))))))
         (free
	  (if pointer?
	      #'dont-free
	      (typecase interface
		((or struct-info union-info)
		 (lambda (position) (declare (ignore position)) t))
		(t #'dont-free))))
	 (gc
	  (if (and pointer? (typep interface 'object-info))
	      (lambda (value transfer)
		(object-setup-gc value transfer) nil)
	      #'dont-gc)))
    (make-converter size set get free gc)))

(defun build-string-converter (type)
  (declare (ignore type))
  (let* ((size (cffi:foreign-type-size :pointer))
         (set
	  (lambda (position value)
	    (let ((pointer (if value (cffi:foreign-string-alloc value)
			       (cffi:null-pointer))))
	      (set-pointer position pointer))))
         (get
	  (lambda (position)
	    (cffi:foreign-string-to-lisp (get-pointer position))))
         (free
	  (lambda (position)
	    (cffi:foreign-free (get-pointer position)))))
    (make-converter size set get free #'dont-gc)))

(defun build-void-converter (type)
  (let* ((pointer? (type-info-is-pointer type))
	 (size (if pointer? (cffi:foreign-type-size :pointer)))
	 (set
	  (if pointer?
	      #'set-pointer
	      (lambda (position value)
		(declare (ignore value))
		(set-pointer position (cffi:null-pointer)))))
         (get
	  (if pointer?
	      #'get-pointer
	      (lambda (position) (declare (ignore position)) nil)))
         (free
	  #'dont-free))
    (make-converter size set get free #'dont-gc)))

(defun build-general-converter (type)
  (let* ((tag (type-info-get-tag type))
         (pointer? (type-info-is-pointer type))
	 (foreign-type (if pointer? :pointer
			   (case tag
			     (:boolean :boolean)
			     (:int8 :int8)
			     (:uint8 :uint8)
			     (:int16 :int16)
			     (:uint16 :uint16)
			     (:int32 :int32)
			     (:uint32 :uint32)
			     (:int64 :int64)
			     (:uint64 :uint64)
			     (:short :short)
			     (:ushort :ushort)
			     (:int :int)
			     (:uint :uint)
			     (:long :long)
			     (:ulong :ulong)
			     (:sszie :long)
			     (:szie :ulong)
			     (:float :float)
			     (:double :double)
			     (:time-t :long)
			     (:gtype :ulong)
			     (:unichar :int32)
			     (t :pointer))))
	 (size (cffi:foreign-type-size foreign-type))
         (set
	  (if pointer?
	      #'set-pointer
	      (lambda (position value)
		(setf (cffi:mem-ref position foreign-type) value))))
         (get
           (if pointer?
	       #'get-pointer
	       (lambda (position)
		 (cffi:mem-ref position foreign-type))))
         (free
	  #'dont-free))
    (make-converter size set get free #'dont-gc)))

(defun build-converter (type)
  (case (type-info-get-tag type)
    (:array (build-array-converter type))
    (:interface (build-interface-converter type))
    ((:utf8 :filename) (build-string-converter type))
    (:void (build-void-converter type))
    (t (build-general-converter type))))
(export 'build-converter)

(defun build-translator (type)
  (let* ((tag (type-info-get-tag type))
         (pointer? (type-info-is-pointer type))
         (field (if pointer? 'v-pointer
		    (case tag
		      (:void 'v-pointer)
		      (:boolean 'v-boolean)
		      (:int8 'v-int8)
		      (:uint8 'v-uint8)
		      (:int16 'v-int16)
		      (:uint16 'v-uint16)
		      (:int32 'v-int32)
		      (:uint32 'v-uint32)
		      (:int64 'v-int64)
		      (:uint64 'v-uint64)
		      (:short 'v-short)
		      (:ushort 'v-ushort)
		      (:int 'v-int)
		      (:uint 'v-uint)
		      (:long 'v-long)
		      (:ulong 'v-ulong)
		      (:ssize 'v-long)
		      (:size 'v-ulong)
		      (:float 'v-float)
		      (:double 'v-double)
		      (:time-t 'v-long)
		      (:gtype 'v-ulong)
		      (:utf8 'v-string)
		      (:interface 'v-uint)
		      (:filename 'v-string)
		      (:unichar 'v-uint32)
		      (t 'v-pointer))))
	 (converter (build-converter type))
         (value->giarg
	  (lambda (giarg value)
	    (funcall (converter-set converter)
		     (cffi:foreign-slot-pointer giarg '(:union argument) field)
		     value)))
         (giarg->value
	  (lambda (giarg &optional length)
	    (let ((position (cffi:foreign-slot-pointer giarg '(:union argument)
						       field))
		  (get (converter-get converter)))
	      (if length
		  (funcall get position length)
		  (funcall get position)))))
         (giarg-free
	  (lambda (giarg &optional length)
	    (let ((position (cffi:foreign-slot-pointer giarg '(:union argument)
						       field))
		  (free (converter-free converter)))
	      (if length
		  (funcall free position length)
		  (funcall free position))))))
    (make-translator value->giarg giarg->value giarg-free
		     (converter-gc converter))))
(export 'build-translator)

(defmacro incf-giargs (giargs)
  `(setf ,giargs (cffi:mem-aptr ,giargs '(:union argument) 1)))

(defun get-array-length (type)
  (let ((length (type-info-get-array-length type)))
    (if (eql length -1) nil length)))

(defstruct arg-processor
  direction
  for-array-length?
  array-length
  setup
  >value
  clear
  gc)

(defvar *obj-arg-processor*
  (make-arg-processor
   :direction :in
   :setup #'pointer->giarg
   :clear #'dont-free))

(defun build-arg-processor (arg)
  (let* ((type (arg-info-get-type arg))
	 (trans (build-translator type))
	 (direction (arg-info-get-direction arg))
	 (transfer (arg-info-get-ownership-transfer arg))
	 (setup (translator->giarg trans))
	 (arg->value (translator->value trans))
	 (clear (ecase direction
		  (:in (translator-free trans))
		  ((:in-out :out)
		   (ecase transfer
		     (:everything (translator-free trans))
		     ((:nothing :container) #'dont-free)))))
	 (gc (lambda (value)
	       (funcall (translator-gc trans) value transfer))))
    (make-arg-processor :direction direction
			:array-length (get-array-length type)
			:setup setup :>value arg->value :clear clear
			:gc gc)))

(defstruct return-processor
  array-length
  >value
  clear
  gc)

(defvar *raw-return-processor*
  (make-return-processor
   :>value #'giarg->pointer
   :clear #'dont-free
   :gc #'proc-dont-gc))

(defun build-return-processor (func-info)
  (let* ((type (callable-info-get-return-type func-info))
	 (transfer (callable-info-get-caller-owns func-info))
	 (trans (build-translator type))
	 (clear (ecase transfer
		  (:everything (translator-free trans))
		  ((:nothing :container) #'dont-free)))
	 (gc (lambda (value)
	       (funcall (translator-gc trans) value transfer))))
    (make-return-processor :array-length (get-array-length type)
			   :>value (translator->value trans) :clear clear
			   :gc gc)))

(defun get-args (info)
  ;; if construct + in-arg
  (let* ((n-args (g-callable-info-get-n-args info))
	 (args-processor
	  (when (method? (function-info-get-flags info))
	    (list *obj-arg-processor*)))
	 (in-count (length args-processor))
	 (out-count 0)
	 (in-array-length-count 0))
    (dotimes (i n-args)
      (let* ((arg (g-callable-info-get-arg info i))
             (direction (arg-info-get-direction arg))
	     (arg-processor (build-arg-processor arg)))
	(ecase direction
	  (:in (incf in-count))
	  (:in-out (incf in-count) (incf out-count))
	  (:out (incf out-count)))
	(push arg-processor args-processor)))
    (setf args-processor (nreverse args-processor))
    (loop
       :for arg-processor :in args-processor
       :for array-length = (arg-processor-array-length arg-processor)
       :when array-length
       :do (setf (arg-processor-for-array-length?
		  (nth array-length args-processor)) t))
    (loop
       :for arg-processor :in args-processor
       :when (and (not (eq (arg-processor-direction arg-processor) :out))
		  (arg-processor-for-array-length? arg-processor))
       :do (incf in-array-length-count))
    (values args-processor in-count out-count in-array-length-count)))

(defstruct
    (arg-state (:constructor make-arg-state (proc)))
  proc giarg value)

(defun make-args-state (args-processor)
  (loop
     :for proc :in args-processor
     :collect (make-arg-state proc)))

(defun check-args (args in-count name)
  (assert (= in-count (length args))
          (args)
          "Should be ~a arguments in function ~a"
	  in-count name))

(defun set-args-state-input (args-state args)
  (loop
     :for i :upfrom 0
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :unless (or (eq (arg-processor-direction proc) :out)
		 (arg-processor-for-array-length? proc))
     :do (let ((arg (car args))
	       (array-length (arg-processor-array-length proc)))
	   (setf (arg-state-value arg-state) arg)
	   (if array-length
	       (setf (arg-state-value (nth array-length args-state))
		     (length arg)))
	   (setf args (cdr args)))))

(defun setup-giargs (args-state giargs-in giargs-out values-out)
  (loop
     :with inp = giargs-in :and outp = giargs-out :and voutp = values-out
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :for dir = (arg-processor-direction proc)
     :for value = (arg-state-value arg-state)
     :do (ecase dir
	   (:in
	    (setf (arg-state-giarg arg-state) inp)
	    (funcall (arg-processor-setup proc) inp value)
	    (incf-giargs inp))
	   (:in-out
	    (setf (arg-state-giarg arg-state) voutp)
	    (funcall (arg-processor-setup proc) voutp value)
	    (pointer->giarg inp voutp)
	    (pointer->giarg outp voutp)
	    (incf-giargs inp)
	    (incf-giargs outp)
	    (incf-giargs voutp))
	   (:out
	    (setf (arg-state-giarg arg-state) voutp)
	    (pointer->giarg outp voutp)
	    (incf-giargs outp)
	    (incf-giargs voutp)))))

(defun make-out (ret-proc giarg-res args-state)
  (loop
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :when (and (not (eq (arg-processor-direction proc) :in))
		(null (arg-processor-array-length proc)))
     :do (let ((value (funcall (arg-processor->value proc)
			       (arg-state-giarg arg-state))))
	   (setf (arg-state-value arg-state) value)))
  (loop
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :for array-length = (arg-processor-array-length proc)
     :when (and array-length (not (eq (arg-processor-direction proc) :in)))
     :do (let* ((length (arg-state-value (nth array-length args-state)))
		(value (funcall (arg-processor->value proc)
				(arg-state-giarg arg-state) length)))
	   (setf (arg-state-value arg-state) value)))
  (cons
   (let ((array-length (return-processor-array-length ret-proc)))
     (if array-length
	 (funcall (return-processor->value ret-proc) giarg-res
		  (arg-state-value (nth array-length args-state)))
	 (funcall (return-processor->value ret-proc) giarg-res)))
   (loop
      :for arg-state :in args-state
      :for proc = (arg-state-proc arg-state)
      :when (and (not (eq (arg-processor-direction proc) :in))
		 (null (arg-processor-for-array-length? proc)))
      :collect (arg-state-value arg-state))))

(defun gc-args (args-state)
  (loop
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :for value = (arg-state-value arg-state)
     :when (and value (not (eq (arg-processor-direction proc) :in)))
     :do (funcall (arg-processor-gc proc) value)))

(defun clear-giargs (args-state)
  (loop
     :for arg-state :in args-state
     :for proc = (arg-state-proc arg-state)
     :for dir = (arg-processor-direction proc)
     :for array-length = (arg-processor-array-length proc)
     :for giarg = (arg-state-giarg arg-state)
     :for value = (arg-state-value arg-state)
     :do (funcall (arg-processor-clear proc) giarg
		  (if array-length (length value)))))

(cffi:defcfun g-function-info-invoke :boolean
  (info info-ffi)
  (in-args :pointer) (n-in-args :int)
  (out-args :pointer) (n-out-args :int)
  (ret :pointer) (g-error :pointer))

(defun build-function (info &key return-raw-pointer)
  (multiple-value-bind (args-processor in-count out-count in-array-length-count)
      (get-args info)
    (let* ((name (info-get-name info))
	   (ret-proc (if return-raw-pointer
			 *raw-return-processor*
			 (build-return-processor info))))
      (lambda (&rest args)
	(let ((args-state (make-args-state args-processor))
	      out)
	  (check-args args (- in-count in-array-length-count) name)
	  (set-args-state-input args-state args)
	  (values-list
	   (cffi:with-foreign-objects
	       ((giargs-in '(:union argument) in-count)
		(giargs-out '(:union argument) out-count)
		(values-out '(:union argument) out-count)
		(giarg-res '(:union argument)))
	     (setup-giargs args-state giargs-in giargs-out values-out)
	     (unwind-protect
		  (with-gerror g-error
		    (g-function-info-invoke info
					    giargs-in in-count
					    giargs-out out-count
					    giarg-res g-error)
		    (setf out (make-out ret-proc giarg-res args-state)))
	       (if (and out (car out))
		   (funcall (return-processor-gc ret-proc) (car out)))
	       (gc-args args-state)
	       (clear-giargs args-state)
	       (funcall (return-processor-clear ret-proc) giarg-res)))))))))
