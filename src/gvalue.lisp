(in-package :gir)

(cffi:defcunion g-value-data
  (v-int :int)
  (v-uint :uint)
  (v-long :long)
  (v-ulong :ulong)
  (v-int64 :int64)
  (v-uint64 :uint64)
  (v-float :float)
  (v-double :double)
  (v-pointer :pointer))

(cffi:defcstruct g-value-struct
  "GValue struct"
  (g-type :ulong)
  (data (:union g-value-data) :count 2))

(cffi:define-foreign-type pvariant ()
  ()
  (:documentation "pointer to GVariant")
  (:actual-type :pointer)
  (:simple-parser pvariant))

(defmethod cffi:translate-to-foreign (object (type pvariant))
  (this-of object))

(defmethod cffi:translate-from-foreign (pointer (type pvariant))
  ;; #define G_TYPE_FUNDAMENTAL_SHIFT (2)
  ;; #define G_TYPE_MAKE_FUNDAMENTAL(x) ((GType) ((x) << G_TYPE_FUNDAMENTAL_SHIFT))
  ;; #define G_TYPE_VARIANT G_TYPE_MAKE_FUNDAMENTAL (21)
  (gobject (ash 21 2) pointer))

(cffi:defcfun g-value-init :void (value :pointer) (gtype :ulong))

(cffi:defcfun g-value-get-boolean :boolean (g-value :pointer))
(cffi:defcfun g-value-get-char :char (g-value :pointer))
(cffi:defcfun g-value-get-uchar :uchar (g-value :pointer))
(cffi:defcfun g-value-get-int :int (g-value :pointer))
(cffi:defcfun g-value-get-uint :uint (g-value :pointer))
(cffi:defcfun g-value-get-long :long (g-value :pointer))
(cffi:defcfun g-value-get-ulong :ulong (g-value :pointer))
(cffi:defcfun g-value-get-int64 :int64 (g-value :pointer))
(cffi:defcfun g-value-get-uint64 :uint64 (g-value :pointer))
(cffi:defcfun g-value-get-float :float (g-value :pointer))
(cffi:defcfun g-value-get-double :double (g-value :pointer))
(cffi:defcfun g-value-get-enum :int (g-value :pointer))
(cffi:defcfun g-value-get-flags :uint (g-value :pointer))
(cffi:defcfun g-value-get-string :string (g-value :pointer))
(cffi:defcfun g-value-get-param :pointer (g-value :pointer))
(cffi:defcfun g-value-get-boxed :pointer (g-value :pointer))
(cffi:defcfun g-value-get-pointer :pointer (g-value :pointer))
(cffi:defcfun g-value-get-object pobject (g-value :pointer))
(cffi:defcfun g-value-get-variant pvariant (g-value :pointer))

(cffi:defcfun g-value-set-boolean :void (g-value :pointer) (val :boolean))
(cffi:defcfun g-value-set-char :void (g-value :pointer) (val :char))
(cffi:defcfun g-value-set-uchar :void (g-value :pointer) (val :uchar))
(cffi:defcfun g-value-set-int :void (g-value :pointer) (val :int))
(cffi:defcfun g-value-set-uint :void (g-value :pointer) (val :uint))
(cffi:defcfun g-value-set-long :void (g-value :pointer) (val :long))
(cffi:defcfun g-value-set-ulong :void (g-value :pointer) (val :ulong))
(cffi:defcfun g-value-set-int64 :void (g-value :pointer) (val :int64))
(cffi:defcfun g-value-set-uint64 :void (g-value :pointer) (val :uint64))
(cffi:defcfun g-value-set-float :void (g-value :pointer) (val :float))
(cffi:defcfun g-value-set-double :void (g-value :pointer) (val :double))
(cffi:defcfun g-value-set-enum :void (g-value :pointer) (val :int))
(cffi:defcfun g-value-set-flags :void (g-value :pointer) (val :uint))
(cffi:defcfun g-value-set-string :void (g-value :pointer) (val :string))
(cffi:defcfun g-value-set-param :void (g-value :pointer) (val :pointer))
(cffi:defcfun g-value-set-boxed :void (g-value :pointer) (val :pointer))
(cffi:defcfun g-value-set-pointer :void (g-value :pointer) (val :pointer))
(cffi:defcfun g-value-set-object :void (g-value :pointer) (val pobject))
(cffi:defcfun g-value-set-variant :void (g-value :pointer) (val pvariant))

(defun g-value-get-interface (g-value) (g-value-get-object g-value))
(defun g-value-set-interface (g-value val) (g-value-set-object g-value val))

(cffi:defcfun g-type-fundamental :ulong (id :ulong))

;; #define G_TYPE_FUNDAMENTAL_SHIFT (2)
;; #define G_TYPE_MAKE_FUNDAMENTAL(x) ((GType) ((x) << G_TYPE_FUNDAMENTAL_SHIFT))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +fundamental-g-types+
    '(:invalid :void :interface :char :uchar :boolean
      :int :uint :long :ulong :int64 :uint64
      :enum :flags :float :double :string
      :pointer :boxed :param :object :variant)))

(defun ffi-enum (value gtype)
  (declare (ignore gtype))
  "Maybe later it will convert list of symbols to integer"
  value)

(macrolet ((build-case (ptr gtype prefix &optional value)
             ;; (case (g-type-fundamental gtype)
             ;;    ...
             ;;    (12 (g-value-set-char ptr value))
             ;;    ...
             (flet ((process (type-name value)
                      (case type-name
                        (:float `(coerce ,value 'single-float))
                        (:double `(coerce ,value 'double-float))
                        ((:int :uint :long :ulong :int64 :uint64)
                         `(round ,value))
                        ((:enum :flags) 
                         `(ffi-enum ,value ,gtype))
                        (t value))))
               `(case (g-type-fundamental gtype)
                  ,@(loop 
                      :for i :from 0
                      :for type-name :in +fundamental-g-types+
                      :when (>= i 2)
                        :collect `(,(* i 4)
                                   (,(intern 
                                      (format nil "~:@(g-value-~a-~a~)"
                                              prefix type-name))
                                    ,ptr ,@(when value 
                                             (list 
                                              (process type-name 
                                                       value))))))))))
  (defun set-value! (ptr gtype value)
    (build-case ptr gtype :set value))
  (defun gvalue->lisp/free (ptr gtype &key no-free)
    (let ((res (build-case ptr gtype :get)))
      (unless no-free
        (cffi:foreign-free ptr))
      res)))

(defun gvalue-gtype (gvalue)
  (cffi:foreign-slot-value gvalue '(:struct g-value-struct) 'g-type))


(defun make-gvalue (gtype &optional (value nil value-p))
  (let* ((ptr (cffi:foreign-alloc '(:struct g-value-struct))))
    (setf (cffi:foreign-slot-value ptr '(:struct g-value-struct) 'g-type) 0)
    (g-value-init ptr gtype)
    (when value-p
      (set-value! ptr gtype value))
    ptr))
