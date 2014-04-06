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
      (:constructor make-converter (size set get free)))
  size set get free)

(defstruct 
    (translator 
      (:constructor make-translator (>giarg >value check description
					    free array-length)))
  >giarg >value check description free array-length)

(defun zero-memory (position length)
  (loop
     :for i :below length
     :for pos = position :then (cffi:inc-pointer pos 1)
     :do (setf (cffi:mem-ref pos :uint8) 0)))

(defun zero? (position length)
  (loop
     :for i :below length
     :for pos = position :then (cffi:inc-pointer pos 1)
     :unless (eql (cffi:mem-ref pos :uint8) 0)
     :do (return-from zero? nil))
  t)

(defun set-pointer (position value)
  (typecase value
    (function (set-pointer position (funcall value :this)))
    (t (setf (cffi:mem-ref position :pointer) value))))

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

(defun build-converter (type)
  (let* ((tag (type-info-get-tag type))
         (pointer? (type-info-is-pointer type))
	 (foreign-type (if pointer? :pointer
			   (case tag
			     (:void :pointer)
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
			     (:utf8 :pointer)
			     (:interface :uint)
			     (:filename :pointer)
			     (:unichar :int32)
			     (t :pointer))))
	 param-type
	 param-converter
	 param-size
	 param-set
	 param-get
	 param-free
	 zero-terminated?
	 fixed-size
	 (array-type (when (eq tag :array)
		       (setf param-type (type-info-get-param-type type 0))
		       (setf param-converter (build-converter param-type))
		       (setf param-size (converter-size param-converter))
		       (setf param-set (converter-set param-converter))
		       (setf param-get (converter-get param-converter))
		       (setf param-free (converter-free param-converter))
		       (setf zero-terminated?
			     (type-info-is-zero-terminated type))
		       (let ((size (type-info-get-array-fixed-size type)))
			 (if (/= -1 size) (setf fixed-size size)))
		       (type-info-get-array-type type)))
	 (size (cffi:foreign-type-size foreign-type))
         (set
	  (if pointer?
	      (case tag
		((:utf8 :filename)
		 (lambda (position value)
		   (let ((pointer (if value (cffi:foreign-string-alloc value)
				      (cffi:null-pointer))))
		     (set-pointer position pointer))))
		(:interface
		 (lambda (position value)
		   (set-pointer position (if value value (cffi:null-pointer)))))
		(:array
		 (case array-type
		   (:c
		    (lambda (position value)
		      (let* ((length (if fixed-size
					 fixed-size
					 (+ (length value)
					    (if zero-terminated? 1 0))))
			     (array (cffi:foreign-alloc
				     :int8 :count (* length param-size))))
			(loop
			   :for pos = array :then (cffi:inc-pointer
						   pos param-size)
			   :for i :below (if zero-terminated?
					     (1- length) length)
			   :for element :in value
			   :do (funcall param-set pos element)
			   :finally (if zero-terminated?
					(zero-memory pos param-size)))
			(set-pointer position array))))
		   (t #'set-pointer)))
		(t #'set-pointer))
	      (case tag
		(:void (lambda (position value)
			 (declare (ignore value))
			 (set-pointer position (cffi:null-pointer))))
		(t (lambda (position value)
		     (setf (cffi:mem-ref position foreign-type) value))))))
         (get
           (if pointer?
               (case tag
                 ((:utf8 :filename)
                   (lambda (position)
                     (cffi:foreign-string-to-lisp
		      (get-pointer position))))
		 (:interface
		  (let* ((info (type-info-get-interface type))
			 (info-type (info-get-type info)))
		    (case info-type
		      ((:struct :object)
		       (let ((struct-object (if (eq info-type :struct)
						(build-struct info)
						(build-object info))))
			 (lambda (position)
			   (let ((this (get-pointer position)))
			     (if (cffi:null-pointer-p this) nil
				 (funcall struct-object this))))))
		      (t #'get-pointer))))
		 (:array
		  (case array-type
		    (:c
		     (lambda (position &optional length)
		       (if fixed-size
			   (if (or (null length) (> length fixed-size))
			       (setf length fixed-size)))
		       (loop
			  :for i :upfrom 0
			  :for pos = (get-pointer position)
			  :then (cffi:inc-pointer pos param-size)
			  :until (and (or (not zero-terminated?) length)
				      (>= i length))
			  :until (and zero-terminated? (zero? pos param-size))
			  :collect (funcall param-get pos))))
		    (t #'get-pointer)))
		 (t #'get-pointer))
               (case tag
                 (:void (lambda (position) (declare (ignore position)) nil))
                 (t (lambda (position)
		      (cffi:mem-ref position foreign-type))))))
         (free
	  (if pointer?
	      (case tag
		((:utf8 :filename)
		 (lambda (position)
		   (cffi:foreign-free (get-pointer position))))
		(:array
		 (case array-type
		   (:c
		    (lambda (position &optional length)
		      (if fixed-size
			  (if (or (null length) (> length fixed-size))
			      (setf length fixed-size)))
		      (let ((array (get-pointer position)))
			(loop
			   :for i :upfrom 0
			   :for pos = array
			   :then (cffi:inc-pointer pos param-size)
			   :until (and (or (not zero-terminated?) length)
				       (>= i length))
			   :until (and zero-terminated? (zero? pos param-size))
			   :do (funcall param-free pos))
			(cffi:foreign-free array))))
		   (t #'dont-free)))
		(t #'dont-free))
              #'dont-free)))
    (make-converter size set get free)))
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
	 (array-length (let ((length (type-info-get-array-length type)))
			 (if (eql length -1) nil length)))
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
		  (funcall free position)))))
         (check-value
          (lambda (value) (declare (ignore value)) t))
         (description (format nil "~a" tag)))
    (make-translator value->giarg giarg->value 
                     check-value description giarg-free array-length)))
(export 'build-translator)

(defvar *raw-pointer-translator*
  (make-translator #'pointer->giarg #'giarg->pointer
		   #'cffi:pointerp "raw pointer"
		   #'dont-free nil))

(defmacro incf-giargs (giargs)
  `(setf ,giargs (cffi:mem-aptr ,giargs '(:union argument) 1)))

(defstruct arg-processor
  direction
  array-length
  setup
  >value
  clear)

(defvar *obj-arg-processor*
  (make-arg-processor
   :direction :in
   :setup #'pointer->giarg
   :clear #'dont-free))

(defun build-arg-processor (arg)
  (let* ((trans (build-translator (arg-info-get-type arg)))
	 (direction (arg-info-get-direction arg))
	 (transfer (arg-info-get-ownership-transfer arg))
	 (setup (translator->giarg trans))
	 (arg->value (translator->value trans))
	 (clear (ecase direction
		  (:in (translator-free trans))
		  ((:in-out :out)
		   (ecase transfer
		     (:everything (translator-free trans))
		     ((:nothing :container) #'dont-free))))))
    (make-arg-processor :direction direction
			:array-length (translator-array-length trans)
			:setup setup :>value arg->value :clear clear)))

(defun get-args (info)
  ;; if construct + in-arg
  (let* ((n-args (g-callable-info-get-n-args info))
	 (args-processor
	  (when (method? (function-info-get-flags info))
	    (list *obj-arg-processor*)))
	 (in-count (length args-processor))
	 (out-count 0))
    (dotimes (i n-args)
      (let* ((arg (g-callable-info-get-arg info i))
             (direction (arg-info-get-direction arg))
	     (arg-processor (build-arg-processor arg)))
	(ecase direction
	  (:in (incf in-count))
	  (:in-out (incf in-count) (incf out-count))
	  (:out (incf out-count)))
	(push arg-processor args-processor)))
    (values (nreverse args-processor) in-count out-count)))

(defun setup-giargs (args-processor in-count out-count args)
  (let ((giargs-in (cffi:foreign-alloc '(:union argument) :count in-count))
	(giargs-out (cffi:foreign-alloc '(:union argument) :count out-count))
	(values-out (cffi:foreign-alloc '(:union argument) :count out-count)))
    (loop
       :with inp = giargs-in :and outp = giargs-out :and voutp = values-out
       :for proc :in args-processor
       :for arg = (if args (car args) nil)
       :for dir = (arg-processor-direction proc)
       :do (ecase dir
	     (:in
	      (funcall (arg-processor-setup proc) inp arg)
	      (incf-giargs inp)
	      (setf args (cdr args)))
	     (:in-out
	      (funcall (arg-processor-setup proc) voutp arg)
	      (pointer->giarg inp voutp)
	      (pointer->giarg outp voutp)
	      (incf-giargs inp)
	      (incf-giargs outp)
	      (incf-giargs voutp)
	      (setf args (cdr args)))
	     (:out
	      (pointer->giarg outp voutp)
	      (incf-giargs outp)
	      (incf-giargs voutp))))
    (values giargs-in giargs-out values-out)))

(defun return-giarg (info)
  (build-translator (callable-info-get-return-type info)))

(cffi:defcfun g-function-info-invoke :boolean
  (info info-ffi) 
  (in-args :pointer) (n-in-args :int)
  (out-args :pointer) (n-out-args :int)
  (ret :pointer) (g-error :pointer))

(defun make-out (res-trans giarg-res args-processor values-out)
  (let* ((all-out-args
	  (loop
	     :with voutp = values-out
	     :for proc in args-processor
	     :for out? = (not (eq (arg-processor-direction proc) :in))
	     :collect (if (and out? (null (arg-processor-array-length proc)))
			  (funcall (arg-processor->value proc) voutp))
	     :when out?
	     :do (incf-giargs voutp)))
	 (out-args
	  (loop
	     :with voutp = values-out
	     :for proc in args-processor
	     :for array-length = (arg-processor-array-length proc)
	     :for out-arg in all-out-args
	     :for out? = (not (eq (arg-processor-direction proc) :in))
	     :when out?
	     :collect (if array-length
			  (funcall (arg-processor->value proc)
				   voutp (nth array-length all-out-args))
			  out-arg)
	     :and :do (incf-giargs voutp)))
	 (return-val
	  (let ((array-length (translator-array-length res-trans)))
	    (if array-length
		(funcall (translator->value res-trans) giarg-res
			 (nth array-length all-out-args))
		(funcall (translator->value res-trans) giarg-res)))))
    (values (cons return-val out-args) all-out-args)))

(defun check-args (args in-count name)
  (assert (= in-count (length args))
          (args) 
          "Should be ~a arguments in function ~a" 
	  in-count name))

(defun clear-giargs (args-processor giargs-in giargs-out values-out
		     args all-out-args)
  (loop
     :with inp = giargs-in :and voutp = values-out
     :for proc :in args-processor
     :for dir = (arg-processor-direction proc)
     :for array-length = (arg-processor-array-length proc)
     :do (ecase dir
	   (:in
	    (funcall (arg-processor-clear proc) inp
		     (if array-length (nth array-length args)))
	    (incf-giargs inp))
	   (:in-out
	    (funcall (arg-processor-clear proc) voutp
		     (if array-length (nth array-length all-out-args)))
	    (incf-giargs inp)
	    (incf-giargs voutp))
	   (:out
	    (funcall (arg-processor-clear proc) voutp
		     (if array-length (nth array-length all-out-args)))
	    (incf-giargs voutp))))
  (cffi:foreign-free giargs-in)
  (cffi:foreign-free giargs-out)
  (cffi:foreign-free values-out))

(defun build-function (info &key return-raw-pointer)
  (multiple-value-bind (args-processor in-count out-count) (get-args info)
    (let ((name (info-get-name info)))
      (lambda (&rest args)
        (check-args args in-count name)
        (values-list
	 (multiple-value-bind (giargs-in giargs-out values-out)
	     (setup-giargs args-processor in-count out-count args)
	   (let ((res-trans (if return-raw-pointer
				*raw-pointer-translator*
				(return-giarg info)))
		 (giarg-res (cffi:foreign-alloc '(:union argument)))
		 all-out-args)
	     (unwind-protect
		  (with-gerror g-error
		    (g-function-info-invoke info
					    giargs-in in-count
					    giargs-out out-count
					    giarg-res g-error)
		    (multiple-value-bind (out all-out)
			(make-out res-trans giarg-res args-processor values-out)
		      (setf all-out-args all-out)
		      out))
               (clear-giargs args-processor giargs-in giargs-out values-out
			     args all-out-args)))))))))
