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

(defun argument->lisp-value (argument length type)
  (case (type-info-get-tag type)
    (:boolean	 (cffi:foreign-slot-value argument 'argument 'v-boolean))
    (:int8	 (cffi:foreign-slot-value argument 'argument 'v-int8))
    (:uint8	 (cffi:foreign-slot-value argument 'argument 'v-uint8))
    (:int16	 (cffi:foreign-slot-value argument 'argument 'v-int16))
    (:uint16	 (cffi:foreign-slot-value argument 'argument 'v-uint16))
    (:int32	 (cffi:foreign-slot-value argument 'argument 'v-int32))
    (:uint32	 (cffi:foreign-slot-value argument 'argument 'v-uint32))
    (:int64	 (cffi:foreign-slot-value argument 'argument 'v-int64))
    (:uint64	 (cffi:foreign-slot-value argument 'argument 'v-uint64))
    (:short	 (cffi:foreign-slot-value argument 'argument 'v-short))
    (:ushort	 (cffi:foreign-slot-value argument 'argument 'v-ushort))
    (:int	 (cffi:foreign-slot-value argument 'argument 'v-int))
    (:uint	 (cffi:foreign-slot-value argument 'argument 'v-uint))
    (:long	 (cffi:foreign-slot-value argument 'argument 'v-long))
    (:ulong	 (cffi:foreign-slot-value argument 'argument 'v-ulong))
    (:ssize	 (cffi:foreign-slot-value argument 'argument 'v-long))
    (:size	 (cffi:foreign-slot-value argument 'argument 'v-ulong))
    (:float	 (cffi:foreign-slot-value argument 'argument 'v-float))
    (:double	 (cffi:foreign-slot-value argument 'argument 'v-double))
    (:time-t	 (cffi:foreign-slot-value argument 'argument 'v-long))
    (:gtype	 (cffi:foreign-slot-value argument 'argument 'v-ulong))
    (:utf8	 (cffi:foreign-slot-value argument 'argument 'v-string))
    (:filename	 (cffi:foreign-slot-value argument 'argument 'v-string))
    #|
    (:array (values (cffi:foreign-slot-value argument 'argument 'v-pointer)
		    length))
    (:interface (cffi:foreign-slot-value argument 'argument 'v-pointer))
    (:glist nil)
    (:gslist nil)
    (:ghash nil)
    (:error nil)
    |#
    (otherwise (error "TODO"))))

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

