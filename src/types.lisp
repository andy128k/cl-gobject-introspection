;; -*- mode: Common-Lisp -*-

(in-package :gir)

(cffi:defcunion argument
    ""
  (v-boolean :boolean)
  (v-int8 :int8)
  (v-uint8 :uint8)
  (v-int16 :int16)
  (v-uint16 :uint16)
  (v-int32 :int32)
  (v-uint32 :uint32)
  (v-int64 :int64)
  (v-uint64 :uint64)
  (v-float :float)
  (v-double :double)
  (v-short :short)
  (v-ushort :ushort)
  (v-int :int)
  (v-uint :uint)
  (v-long :long)
  (v-ulong :ulong)
  (v-ssize :int)
  (v-size :uint)
  (v-string :string)
  (v-pointer :pointer))
(export 'argument)

(defun argument->lisp-value (argument length type)
  (declare (ignore length))
  (cffi:foreign-slot-value 
   argument '(:union argument)
   (case (type-info-get-tag type)
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
     (:filename 'v-string)
     (t 'v-pointer))))
    #|
    (:array (values (cffi:foreign-slot-value argument 'argument 'v-pointer)
		    length))
    (:interface (cffi:foreign-slot-value argument 'argument 'v-pointer))
    (:glist nil)
    (:gslist nil)
    (:ghash nil)
    (:error nil)
    |#
;     (error "TODO"))))

(cffi:defcenum info-type
    "Types of objects registered in the repository"
  (:invalid 0)
  :function
  :callback
  :struct
  :boxed
  :enum
  :flags
  :object
  :interface
  :constant
  :error-domain
  :union
  :value
  :signal
  :vfunc
  :property
  :field
  :arg
  :type
  :unresolved)

(cffi:defctype gtype :ulong)

(defun gtype (obj) 
  (cffi:mem-ref (cffi:mem-ref obj :pointer) 'gtype))