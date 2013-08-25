(in-package :gir)

(defun method? (flags)
  (let (res)
    (dolist (flag flags res)
      (case flag 
        (:is-constructor (return-from method? nil))
        (:is-method (setf res t))))))

(defstruct 
    (translator 
      (:constructor make-translator (>giarg >value check description)))
  >giarg >value check description)

(defun pointer->giarg (giarg value)
  (typecase value
    (function (pointer->giarg giarg (funcall value :this)))
    (t (setf 
        (cffi:foreign-slot-value giarg '(:union argument) 'v-pointer) 
        value))))

(defun giarg->pointer (giarg)
  (cffi:foreign-slot-value giarg '(:union argument) 'v-pointer))

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
                  (t 'v-pointer)))
         (value->giarg
           (if pointer?
               (case tag
                 ((:utf8 :filename)
                  (lambda (giarg value)
                    (setf (cffi:foreign-slot-value 
                           giarg '(:union argument) 'v-string) 
                          value)))
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
                     (cffi:foreign-slot-value 
                      giarg '(:union argument) 'v-string)))
                 (t #'giarg->pointer))
               (case tag
                 (:void (lambda (giarg) (declare (ignore giarg)) nil))
                 (t (lambda (giarg)
                      (cffi:foreign-slot-value 
                       giarg '(:union argument) field))))))
         (check-value
          (lambda (value) (declare (ignore value)) t))
         (description (format nil "~a" tag)))
    (make-translator value->giarg giarg->value check-value description)))
(export 'build-translator)


(defun get-args (info)
  ;; if construct + in-arg
  (let ((n-args (g-callable-info-get-n-args info))
        (in (when (method? (function-info-get-flags info))
              (list (make-translator #'pointer->giarg #'giarg->pointer 
                                     #'cffi:pointerp "instance pointer"))))
        out)
    (dotimes (i n-args)
      (let* ((arg (g-callable-info-get-arg info i))
             (type (arg-info-get-type arg))
             (direction (arg-info-get-direction arg))
             (builder (build-translator type)))
        (when (member direction '(:in :in-out)) (push builder in))
        (when (member direction '(:out :in-out)) (push builder out))))
    (values (nreverse in) (nreverse out))))

(defun giargs (translators &optional values)
  (let ((ptr (cffi:foreign-alloc '(:union argument) 
                                 :count (length translators))))
    (when values
      (loop 
         :for trans :in translators
         :for val :in values
         :for i :from 0
         :do (funcall (translator->giarg trans) 
                      (cffi:mem-aptr ptr '(:union argument) i)
                      val)))
    ptr))
(export 'giargs)

(defun return-giarg (info)
  (build-translator (callable-info-get-return-type info)))

(cffi:defcfun g-function-info-invoke :boolean
  (info info-ffi) 
  (in-args :pointer) (n-in-args :int)
  (out-args :pointer) (n-out-args :int)
  (ret :pointer) (g-error :pointer))

(defun make-out (res-trans giarg-res out-translators giargs-out)
  (cons 
   (funcall (translator->value res-trans) giarg-res)
   (iter
     (for trans in out-translators)
     (for i from 0)
     (collect (funcall (translator->value trans)
                       (cffi:mem-aptr giargs-out '(:union argument) i))))))
(export 'make-out)

(defun check-args (args in-trans name)
  (assert (= (length in-trans) (length args))
          (args) 
          "Should be ~a arguments in function ~a" 
          (length in-trans) name))

(defun build-function (info)
  (multiple-value-bind (in-trans out-trans) (get-args info)
    (let ((name (info-get-name info)))
      (lambda (&rest args)
        (check-args args in-trans name)
        (values-list 
         (let ((giargs-in (giargs in-trans args))
               (giargs-out (giargs out-trans))
               (res-trans (return-giarg info))
               (giarg-res (cffi:foreign-alloc '(:union argument))))
           ;(format t "DEBUG ~a ~a ~a~%" name args in-trans)
           (with-gerror g-error
             (g-function-info-invoke info
                                     giargs-in (length in-trans) 
                                     giargs-out (length out-trans) 
                                     giarg-res g-error)
             (make-out res-trans giarg-res out-trans giargs-out))))))))