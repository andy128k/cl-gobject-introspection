(in-package :gir)

(defun method? (flags)
  (let (res)
    (dolist (flag flags res)
      (case flag 
        (:is-constructor (return-from method? nil))
        (:is-method (setf res t))))))

(defun constructor? (flags)
  (member :is-constructor flags))

(defstruct 
    (translator 
      (:constructor make-translator (>giarg >value check description free)))
  >giarg >value check description free)

(defun pointer->giarg (giarg value)
  (typecase value
    (function (pointer->giarg giarg (funcall value :this)))
    (t (setf 
        (cffi:foreign-slot-value giarg '(:union argument) 'v-pointer) 
        value))))

(defun giarg->pointer (giarg)
  (cffi:foreign-slot-value giarg '(:union argument) 'v-pointer))

(defun dont-free (giarg)
  (declare (ignore giarg)))

(defun build-translator (type)
  (let* ((tag (type-info-get-tag type))
         (pointer? (type-info-is-pointer type))
         (field (case tag
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
                  (:interface 'v-int)
                  (:filename 'v-string)
		  (:unichar 'v-uint32)
                  (t 'v-pointer)))
         (value->giarg
           (if pointer?
               (case tag
                 ((:utf8 :filename)
                  (lambda (giarg value)
		    (let ((pointer (if value (cffi:foreign-string-alloc value)
				       (cffi:null-pointer))))
		      (setf (cffi:foreign-slot-value
			     giarg '(:union argument) 'v-string) pointer))))
		 (:interface
		  (lambda (giarg value)
		    (pointer->giarg giarg
				    (if value value (cffi:null-pointer)))))
                 (t #'pointer->giarg))
               (case tag
                 (:void (lambda (giarg value)
                          (declare (ignore value))
                          (setf (cffi:foreign-slot-value 
                                 giarg '(:union argument) 'v-pointer) 
                                (cffi:null-pointer))))
                 (t (lambda (giarg value)
                      (setf (cffi:foreign-slot-value 
                             giarg '(:union argument) field) 
                            value))))))
         (giarg->value
           (if pointer?
               (case tag
                 ((:utf8 :filename)
                   (lambda (giarg)
                     (cffi:foreign-string-to-lisp 
                      (cffi:foreign-slot-value 
                       giarg '(:union argument) 'v-string))))
		 (:interface
		  (lambda (giarg)
		    (let ((this (cffi:foreign-slot-value
				 giarg '(:union argument) 'v-pointer))
			  (info (type-info-get-interface type)))
		      (case (info-get-type info)
			(:struct
			 (if (cffi:null-pointer-p this)
			     nil
			     (funcall (build-struct info) this)))
			(:object
			 (if (cffi:null-pointer-p this)
			     nil
			     (funcall (build-object info) this)))
			(t this)))))
                 (t #'giarg->pointer))
               (case tag
                 (:void (lambda (giarg) (declare (ignore giarg)) nil))
                 (t (lambda (giarg)
                      (cffi:foreign-slot-value 
                       giarg '(:union argument) field))))))
         (giarg-free
          (if (and pointer? (member tag '(:utf8 :filename)))
              (lambda (giarg) (cffi:foreign-free 
                               (cffi:foreign-slot-value 
                                giarg '(:union argument) 'v-string)))
              #'dont-free))
         (check-value
          (lambda (value) (declare (ignore value)) t))
         (description (format nil "~a" tag)))
    (make-translator value->giarg giarg->value 
                     check-value description giarg-free)))
(export 'build-translator)

(defvar *raw-pointer-translator*
  (make-translator #'pointer->giarg #'giarg->pointer
		   #'cffi:pointerp "raw pointer"
		   #'dont-free))

(defmacro incf-giargs (giargs)
  `(setf ,giargs (cffi:mem-aptr ,giargs '(:union argument) 1)))

(defstruct arg-processor
  direction
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
    (make-arg-processor :direction direction :setup setup
			:>value arg->value :clear clear)))

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
  (cons
   (funcall (translator->value res-trans) giarg-res)
   (loop
      :with voutp = values-out
      :for proc in args-processor
      :for direction = (arg-processor-direction proc)
      :when (member direction '(:in-out :out))
      :collect (funcall (arg-processor->value proc) voutp)
      :and :do (incf-giargs voutp))))

(defun check-args (args in-count name)
  (assert (= in-count (length args))
          (args) 
          "Should be ~a arguments in function ~a" 
	  in-count name))

(defun clear-giargs (args-processor giargs-in giargs-out values-out)
  (loop
     :with inp = giargs-in :and voutp = values-out
     :for proc :in args-processor
     :for dir = (arg-processor-direction proc)
     :do (ecase dir
	   (:in
	    (funcall (arg-processor-clear proc) inp)
	    (incf-giargs inp))
	   (:in-out
	    (funcall (arg-processor-clear proc) voutp)
	    (incf-giargs inp)
	    (incf-giargs voutp))
	   (:out
	    (funcall (arg-processor-clear proc) voutp)
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
		 (giarg-res (cffi:foreign-alloc '(:union argument))))
           (unwind-protect
                (with-gerror g-error
                  (g-function-info-invoke info
                                          giargs-in in-count
                                          giargs-out out-count
                                          giarg-res g-error)
                  (make-out res-trans giarg-res args-processor values-out))
               (clear-giargs args-processor giargs-in giargs-out values-out)))))))))
