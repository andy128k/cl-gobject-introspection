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
  (v-string :pointer) ;; if :string, it frees pointer after setting it
  (v-pointer :pointer))

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

;; Referencing [this GObject Introspection API page][1] for the union
;; members
;;
;; * We are only interested in unsigned types
;; * We need to determine which of these matches the size of GType
;;
;; These are all the wrong type:
;;
;; - gboolean v_boolean;
;; - gchar *v_string;
;; - gpointer v_pointer;
;; - gdouble v_double;
;; - gfloat v_float;
;;
;; These are all signed:
;; - gint8 v_int8;
;; - gint16 v_int16;
;; - gint32 v_int32;
;; - gint64 v_int64;
;; - gshort v_short;
;; - gint v_int;
;; - glong v_long;
;; - gssize v_ssize;
;;
;; These are the candidates:
;; - gsize v_size;
;; - guint v_uint;
;; - guint16 v_uint16;
;; - guint32 v_uint32;
;; - guint64 v_uint64;
;; - guint8 v_uint8;
;; - gulong v_ulong;
;; - gushort v_ushort;
;;
;; Can there be multiple matches within this candidates list? I think
;; so... If they all share sign and bit-length, though, I think they
;; are compatible members of the union... Undefined behavior? Unsure,
;; but I think it's safe enough
;;
;; [1]: https://gnome.pages.gitlab.gnome.org/gobject-introspection/girepository/gi-Common-Types.html#GIArgument

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun determine-gtype-type-and-union-member ()
    (let ((uintptr
	    (find-if
	     #'(lambda (type) (eq
			       (cffi:foreign-type-size type)
			       (cffi:foreign-type-size :pointer)))
	     (list :unsigned-int
		   :unsigned-long
		   #-cffi-sys::no-long-long
		   :unsigned-long-long))))
      (cond
	((>
	  (cffi:foreign-type-size :pointer)
	  (cffi:foreign-type-size :size))
	 uintptr)
	((neq
	  (cffi:foreign-type-size :size)
	  (cffi:foreign-type-size :long))
	 :size)
	(t (values :usigned-long-long 'v-uint64))))))

(cffi:defctype gtype :ulong)

(defun gtype (obj) 
  (cffi:mem-ref (cffi:mem-ref obj :pointer) 'gtype))
