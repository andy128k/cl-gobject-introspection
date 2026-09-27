(in-package :gir)

(cffi:defcunion argument
  "See [the upstream gobject-introspection/girepository/gitypes.h][1]

[1]: https://gitlab.gnome.org/GNOME/gobject-introspection/-/blob/0cd7f4f39714f3c01d666d165ae1f92cb5c78811/girepository/gitypes.h#L189"
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

(eval-when (:compile-toplevel)
  (defun inspect-gi-argument-union ()
    "Return a plist member types and names from the CFFI GIArgument union

NOTE: How confident are we that these internal functions and symbols
are fit for use?"
    (let ((members
            (the hash-table
		 (slot-value (cffi::parse-type '(:union argument))
                             'cffi::slots))))
      (loop for member being the hash-value of members
            nconc (with-slots (cffi::type cffi::name)
                      member
                    (list cffi::type cffi::name)))))

  (defun gi-argument-union-members ()
    (inspect-gi-argument-union))

  (defun filter-gi-argument-union-members-for-gtype-candidates (members)
    "Determine union members which might be a match for GType

Makes the following assumptions

1. CFFI::CANONICALIZE-FOREIGN-TYPE is stable enough for use
2. The canonicalized type will start with unsigned when it is
   unsigned. I believe that this is guaranteed as part of C
3. We are only interested in unsigned types
4. There may be multiple matches in the candidate list and that that
   is *alright* because if they share signedness and size then they
   are compatible

   * TODO: Verify the safety and definedness of this asserted behavior"
    (loop for (type member) on members by #'cddr
          for canonicalized-type = (cffi::canonicalize-foreign-type type)
          for first-type-component = (let ((name (symbol-name canonicalized-type)))
                                       (subseq name 0 (position #\- name)))
          when (string= first-type-component "UNSIGNED")
            nconc (list type member)))

  (defun determine-gtype ()
    "Determine the CFFI type for GType

The determination of `uintptr` is based on the description in the C99
N1256 working draft from [here][1]. See `7.18.1.4 Integer types
capable of holding object pointers`

> The following type designates an unsigned integer type with the
> property that anyvalid pointer to void can be converted to this
> type, then converted back to pointer to void, and the result will
> compare equal to the original pointer:

The determination of `GType` replicates the C preprocessor logic from
[this portion of glib/gobject/gtype.h][2]

[1]: https://www.open-std.org/jtc1/sc22/wg14/www/projects.html
[2]: https://gitlab.gnome.org/GNOME/glib/-/blob/36c60f069c6f3776dafc7f6ce18c8c0b606cd8b5/gobject/gtype.h#L418"
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
        ((not
          (eq
           (cffi:foreign-type-size :size)
           (cffi:foreign-type-size :long)))
         :size)
        (t :unsigned-long))))

  (defun determine-gi-argument-union-member-for-gtype ()
    (let* ((gtype (determine-gtype))
           (members (filter-gi-argument-union-members-for-gtype-candidates
                     (gi-argument-union-members)))
           (member
             (multiple-value-list
              (loop for (type name) on members by #'cddr
                    when (eq
                          (cffi::canonicalize-foreign-type (determine-gtype))
                          (cffi::canonicalize-foreign-type type))
                      return (values type name)))))
      (unless member
        (error "Could not determine GObject Introspection argument union member from GType (~A) (~A)"
               gtype (cffi::canonicalize-foreign-type gtype)))
      (values-list member))))

(defconstant +gtype+ #.(determine-gtype))
(defconstant +gi-argument-gtype-member-name+
  (quote #.(second
            (multiple-value-list
             (determine-gi-argument-union-member-for-gtype)))))

(macrolet ((defgtype ()
             `(cffi:defctype gtype ,+gtype+)))
  (defgtype))

(defun gtype (obj) 
  (cffi:mem-ref (cffi:mem-ref obj :pointer) 'gtype))
