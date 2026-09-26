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

;; Referencing [this C source for][1] for the union members
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
;; The remaining union members are the candidates and shown below
;;
;; Can there be multiple matches within this candidates list? I think
;; so... If they all share sign and bit-length, though, I think they
;; are compatible members of the union... Undefined behavior? Unsure,
;; but I think it's safe enough
;;
;; I will make the assumption that these types translate
;; straightforwardly to the suggested C types and manually translate
;; them accordingly
;;
;; [1]: https://gitlab.gnome.org/GNOME/gobject-introspection/-/blob/0cd7f4f39714f3c01d666d165ae1f92cb5c78811/girepository/gitypes.h#L189

(defvar *c-union-definition*
  "
union _GIArgument
{
  gboolean v_boolean;
  gint8    v_int8;
  guint8   v_uint8;
  gint16   v_int16;
  guint16  v_uint16;
  gint32   v_int32;
  guint32  v_uint32;
  gint64   v_int64;
  guint64  v_uint64;
  gfloat   v_float;
  gdouble  v_double;
  gshort   v_short;
  gushort  v_ushort;
  gint     v_int;
  guint    v_uint;
  glong    v_long;
  gulong   v_ulong;
  gssize   v_ssize;
  gsize    v_size;
  gchar *  v_string;
  gpointer v_pointer;
};
")

(defun read-union (&aux in-union)
  "Read members of the _GIArgument union into a list

Make sure to unintern symbols to avoid polluting the `gir` package

A few assumptions have been made:

1. Pointer members are both identifiable and easily ignorable
2. CFFI recognizes a type identified by removing the `g` prefix from
   the member type specifier then interning as a keyword"
  (labels
      ((translate-type (input-type-symbol)
	 (intern (subseq (symbol-name input-type-symbol) 1)
		 "KEYWORD"))
       (translate-member (input-member-symbol)
	 (intern (format
		  nil "V-~A"
		  (subseq (symbol-name input-member-symbol) 2))))
       (definitely-read-member (line)
	 (let ((fields
		 (with-input-from-string (stream line)
		   (loop for field = (read stream nil)
			 while field
			 collect field))))
	   (case (length fields)
	     (2 (destructuring-bind (in-type in-member)
		    fields
		  (let ((type (translate-type in-type))
			(member (translate-member in-member)))
		    (unintern in-type)
		    (unintern in-member)
		    (values type member))))
	     (3
	      (if (not (eq (nth 1 fields) '*))
		  (error "Unexpected case"))
	      (loop for symbol in fields
		    do (unintern symbol))
	      (values)))))
       (possibly-read-member (line)
	 (cond
	   ((not in-union)
	    (when (eq (aref line 0) #\{)
	      (setf in-union t))
	    (values))
	   (in-union
	    (cond
	      ((eq (aref line 0) #\})
	       (values))
	      (t (definitely-read-member line)))))))
    (with-input-from-string (stream *c-union-definition*)
      (loop for line = (read-line stream nil)
	    while line
	    for possible-member = (multiple-value-list (possibly-read-member line))
	    when possible-member collect it))))

(defun filtered-union-members ()
  "Determine union members which might be a match for GType

Makes the following assumptions

1. CFFI::CANONICALIZE-FOREIGN-TYPE is stable enough for use
2. The canonicalized will start with unsigned when it is unsigned. I
   believe that this is guaranteed as part of C"
  (loop for (type member) on (read-union) by #'cddr
	for canonicalized-type = (cffi::canonicalize-foreign-type type)
	for first-type-component = (let ((name (symbol-name canonicalized-type)))
				     (subseq name 0 (position #\- name)))
	when (string= first-type-component "UNSIGNED")
	  nconc (list type member)))

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
