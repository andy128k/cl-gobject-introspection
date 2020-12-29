;; -*- mode: Common-Lisp -*-

(in-package :gir)

(defstruct repository
  ptr)

(cffi:define-foreign-type repository-type ()
  ()
  (:actual-type :pointer))
(cffi:define-parse-method repository-type (&key)
  (make-instance 'repository-type))

(defmethod cffi:translate-to-foreign (info (type repository-type))
  (slot-value info 'ptr))
(defmethod cffi:translate-to-foreign ((info null) (type repository-type))
  (cffi:null-pointer))
(defmethod cffi:translate-from-foreign (pointer (type repository-type))
  (make-repository :ptr pointer))

(cffi:defbitfield load-flags
    :lazy)

(progn
  (cffi:defcfun g-irepository-get-type :pointer)
  (cffi:defcfun g-object-new-with-properties :pointer
    (g-type :pointer)
    (n-properties :int)
    (names :pointer)
    (values :pointer))
  
  (defun repository-new ()
    (make-repository :ptr
		     (g-object-new-with-properties (g-irepository-get-type)
						   0
						   (cffi:null-pointer)
						   (cffi:null-pointer)))))


(cffi:defcfun (repository-get-default "g_irepository_get_default") repository-type)


(cffi:defcfun (repository-prepend-search-path "g_irepository_prepend_search_path") :void
  (directory :string))


(progn
  (cffi:defcfun g-irepository-get-search-path :pointer)
  
  (defun repository-get-search-path ()
    (g-slist-to-list (g-irepository-get-search-path) :free nil)))


(progn
  (cffi:defcfun g-irepository-load-typelib :string
    (repository repository-type)
    (typelib typelib-type)
    (flags load-flags)
    (gerror :pointer))
  
  (defun repository-load-typelib (repository typelib &optional flags)
    (with-gerror err
      (g-irepository-load-typelib repository typelib flags err))))


(cffi:defcfun (repository-is-registered "g_irepository_is_registered") :boolean
  (repository repository-type)
  (namespace :string)
  (version :string))


(def-info-func (repository-find-by-name g-irepository-find-by-name)
  (repository repository-type)
  (namespace :string)
  (name :string))


(progn
  (cffi:defcfun g-irepository-require typelib-type
    (repository repository-type)
    (namespace :string)
    (version :string)
    (flags load-flags)
    (gerror :pointer))
  
  (defun repository-require (repository namespace version &optional flags)
    (with-gerror err (g-irepository-require repository namespace version flags err))))


(cffi:defcfun (repository-get-dependencies "g_irepository_get_dependencies") strv-ffi
  (repository repository-type)
  (namespace :string))


(cffi:defcfun (repository-get-loaded-namespaces "g_irepository_get_loaded_namespaces") strv-ffi
  (repository repository-type))


(def-info-func (repository-find-by-gtype g-irepository-find-by-gtype)
  (repository repository-type)
  (g-type :ulong))

(progn
  (cffi:defcfun g-irepository-get-n-infos :int
    (repository repository-type)
    (namespace :string))
  (cffi:defcfun g-irepository-get-info info-ffi
    (repository repository-type)
    (namespace :string)
    (n :int))

  (defun repository-get-infos (repository namespace)
    (let ((n (g-irepository-get-n-infos repository namespace)))
      (iter (for i from 0 below n)
	    (collect (info-ffi-finalize
		      (g-irepository-get-info repository namespace i)))))))

(cffi:defcfun (repository-get-typelib-path "g_irepository_get_typelib_path") :string
  (repository repository-type)
  (namespace :string))

(cffi:defcfun (repository-get-shared-library "g_irepository_get_shared_library") :string
  (repository repository-type)
  (namespace :string))

(cffi:defcfun (repository-get-c-prefix "g_irepository_get_c_prefix") :string
  (repository repository-type)
  (namespace :string))

(cffi:defcfun (repository-get-version "g_irepository_get_version") :string
  (repository repository-type)
  (namespace :string))

;; GOptionGroup * g_irepository_get_option_group (void);
;; gboolean       g_irepository_dump  (const char *arg, GError **error);


#|
void gi_cclosure_marshal_generic (GClosure       *closure,
                                  GValue         *return_gvalue,
                                  guint           n_param_values,
                                  const GValue   *param_values,
                                  gpointer        invocation_hint,
                                  gpointer        marshal_data);
|#

;;;
;;; callable-info
;;;

(def-info-func callable-info-get-return-type
  (callable-info info-ffi))

(cffi:defcenum transfer
    "Represent the transfer ownership information of a callable-info or a arg-info."
  :nothing
  :container
  :everything)

(cffi:defcfun (callable-info-get-caller-owns "g_callable_info_get_caller_owns") transfer
  (callable-info info-ffi))

(cffi:defcfun (callable-info-may-return-null "g_callable_info_may_return_null") :boolean
  (callable-info info-ffi))

(define-collection-getter callable-info-get-args
    g-callable-info-get-n-args g-callable-info-get-arg)

;;;
;;; arg-info
;;;

(cffi:defcenum direction
    "The direction of a arg-info."
  :in
  :out
  :in-out)

(cffi:defcenum scope-type
    "Scope type of a #GIArgInfo representing callback, determines how the
     callback is invoked and is used to decided when the invoke structs
     can be freed."
  :invalid
  :call
  :async
  :notified)

(cffi:defcfun (arg-info-get-direction "g_arg_info_get_direction") direction
  (arg-info info-ffi))

(cffi:defcfun (arg-info-is-return-value "g_arg_info_is_return_value") :boolean
  (arg-info info-ffi))

(cffi:defcfun (arg-info-is-optional "g_arg_info_is_optional") :boolean
  (arg-info info-ffi))

(cffi:defcfun (arg-info-is-caller-allocates "g_arg_info_is_caller_allocates") :boolean
  (arg-info info-ffi))

(cffi:defcfun (arg-info-may-be-null "g_arg_info_may_be_null") :boolean
  (arg-info info-ffi))

(cffi:defcfun (arg-info-get-ownership-transfer "g_arg_info_get_ownership_transfer") transfer
  (arg-info info-ffi))

(cffi:defcfun (arg-info-get-scope "g_arg_info_get_scope") scope-type
  (arg-info info-ffi))

(cffi:defcfun (arg-info-get-closure "g_arg_info_get_closure") :int
  (arg-info info-ffi))

(cffi:defcfun (arg-info-get-destroy "g_arg_info_get_destroy") :int
  (arg-info info-ffi))

(def-info-func arg-info-get-type
  (arg-info info-ffi))

;;;
;;; type-info
;;;

(cffi:defcenum type-tag
    "The type tag of a type-info."
  ;; basic types
  (:void 0)
  :boolean
  :int8
  :uint8
  :int16
  :uint16
  :int32
  :uint32
  :int64
  :uint64
  :float
  :double
  :gtype
  :utf8
  :filename
  ;; non-basic types
  :array
  :interface
  :glist
  :gslist
  :ghash
  :error
  :unichar)

(cffi:defcenum array-type
    "The type of array in a type-info."
  :c
  :array
  :ptr-array
  :byte-array)

(cffi:defcfun (type-tag-to-string "g_type_tag_to_string") :string
  (tag type-tag))

(cffi:defcfun (type-info-is-pointer "g_type_info_is_pointer") :boolean
  (type-info info-ffi))

(cffi:defcfun (type-info-get-tag "g_type_info_get_tag") type-tag
  (type-info info-ffi))

(def-info-func type-info-get-param-type
  (type-info info-ffi)
  (n :int))

(def-info-func type-info-get-interface
  (type-info info-ffi))

(cffi:defcfun (type-info-get-array-length "g_type_info_get_array_length") :int
  (type-info info-ffi))

(cffi:defcfun (type-info-get-array-fixed-size "g_type_info_get_array_fixed_size") :int
  (type-info info-ffi))

(cffi:defcfun (type-info-is-zero-terminated "g_type_info_is_zero_terminated") :boolean
  (type-info info-ffi))

(cffi:defcfun (type-info-get-array-type "g_type_info_get_array_type") array-type
  (type-info info-ffi))

;(define-collection-getter type-info-get-error-domains
;    g-type-info-get-n-error-domains g-type-info-get-error-domain)

;;;
;;; error-domain-info
;;;

;; (cffi:defcfun (error-domain-info-get-quark "g_error_domain_info_get_quark") :string
;;   (error-domain-info info-ffi))
;; (export 'error-domain-info-get-quark)

;; (cffi:defcfun (error-domain-info-get-codes "g_error_domain_info_get_codes") info-ffi
;;   (error-domain-info info-ffi))
;; (export 'error-domain-info-get-codes)

;;;
;;; value-info
;;;

(cffi:defcfun (value-info-get-value "g_value_info_get_value") :long
  (value-info info-ffi))

;;;
;;; field-info
;;;

(cffi:defbitfield field-info-flags
    "Flags for a field-info."
  :readable
  :writable)

(cffi:defcfun (field-info-get-flags "g_field_info_get_flags") field-info-flags
  (field-info info-ffi))

(cffi:defcfun (field-info-get-size "g_field_info_get_size") :int
  (field-info info-ffi))

(cffi:defcfun (field-info-get-offset "g_field_info_get_offset") :int
  (field-info info-ffi))

(def-info-func field-info-get-type
  (field-info info-ffi))

#|
gboolean g_field_info_get_field (GIFieldInfo     *field_info,
				 gpointer         mem,
				 GArgument       *value);
gboolean g_field_info_set_field (GIFieldInfo     *field_info,
				 gpointer         mem,
				 const GArgument *value);
|#

;;;
;;; union-info
;;;

(define-collection-getter union-info-get-fields
    g-union-info-get-n-fields g-union-info-get-field)

(define-collection-getter union-info-get-methods
    g-union-info-get-n-methods g-union-info-get-method)

(cffi:defcfun (union-info-is-discriminated "g_union_info_is_discriminated") :boolean
  (union-info info-ffi))

(cffi:defcfun (union-info-get-discriminator-offset "g_union_info_get_discriminator_offset") :int
  (union-info info-ffi))

(def-info-func union-info-get-discriminator-type
  (union-info info-ffi))

(define-collection-getter union-info-get-discriminators
    (g-union-info-get-n-fields :already-defined) g-union-info-get-discriminator)

(def-info-func union-info-find-method
  (union-info info-ffi)
  (name :string))

(cffi:defcfun (union-info-get-size "g_union_info_get_size") :int
  (union-info info-ffi))

(cffi:defcfun (union-info-get-alignment "g_union_info_get_alignment") :int
  (union-info info-ffi))

;;;
;;; struct-info
;;;

(define-collection-getter struct-info-get-fields
    g-struct-info-get-n-fields g-struct-info-get-field)

(define-collection-getter struct-info-get-methods
    g-struct-info-get-n-methods g-struct-info-get-method)

(def-info-func struct-info-find-method
  (struct-info info-ffi)
  (name :string))

(cffi:defcfun (struct-info-get-size "g_struct_info_get_size") :int
  (struct-info info-ffi))

(cffi:defcfun (struct-info-get-alignment "g_struct_info_get_alignment") :int
  (struct-info info-ffi))

(cffi:defcfun (struct-info-is-gtype-struct "g_struct_info_is_gtype_struct") :boolean
  (struct-info info-ffi))

(cffi:defcfun (struct-info-is-foreign "g_struct_info_is_foreign") :boolean
  (struct-info info-ffi))

;;;
;;; registered-type-info
;;;

(cffi:defcfun (registered-type-info-get-type-name "g_registered_type_info_get_type_name") :string
  (registered-type-info info-ffi))

(cffi:defcfun (registered-type-info-get-type-init "g_registered_type_info_get_type_init") :string
  (registered-type-info info-ffi))

(cffi:defcfun (registered-type-info-get-g-type "g_registered_type_info_get_g_type") :int
  (registered-type-info info-ffi))

;;;
;;; enum-info
;;;

(define-collection-getter enum-info-get-values
    g-enum-info-get-n-values g-enum-info-get-value)

(define-collection-getter enum-info-get-methods
    g-enum-info-get-n-methods g-enum-info-get-method)


(cffi:defcfun (enum-info-get-storage-type "g_enum_info_get_storage_type") info-ffi
  (enum-info info-ffi))

;;;
;;; object-info
;;;

(cffi:defcfun (object-info-get-type-name "g_object_info_get_type_name") :string
  (object-info info-ffi))

(cffi:defcfun (object-info-get-type-init "g_object_info_get_type_init") :string
  (object-info info-ffi))

(cffi:defcfun (object-info-get-abstract "g_object_info_get_abstract") :boolean
  (object-info info-ffi))

(def-info-func object-info-get-parent
  (object-info info-ffi))

(define-collection-getter object-info-get-interfaces
    g-object-info-get-n-interfaces g-object-info-get-interface)

(define-collection-getter object-info-get-fields
    g-object-info-get-n-fields g-object-info-get-field)

(define-collection-getter object-info-get-properties
    g-object-info-get-n-properties g-object-info-get-property)

(define-collection-getter object-info-get-methods
    g-object-info-get-n-methods g-object-info-get-method)

(def-info-func object-info-find-method
  (object-info info-ffi)
  (name :string))

(define-collection-getter object-info-get-signals
    g-object-info-get-n-signals g-object-info-get-signal)

(def-info-func object-info-find-signal
  (object-info info-ffi)
  (name :string))

(define-collection-getter object-info-get-vfuncs
    g-object-info-get-n-vfuncs g-object-info-get-vfunc)

(def-info-func object-info-find-vfunc
  (object-info info-ffi)
  (name :string))

(define-collection-getter object-info-get-constants
    g-object-info-get-n-constants g-object-info-get-constant)

(def-info-func object-info-get-class-struct
  (object-info info-ffi))

;;;
;;; interface-info
;;;

(define-collection-getter interface-info-get-prerequisites
    g-interface-info-get-n-prerequisites g-interface-info-get-prerequisite)

(define-collection-getter interface-info-get-properties
    g-interface-info-get-n-properties g-interface-info-get-property)

(define-collection-getter interface-info-get-methods
    g-interface-info-get-n-methods g-interface-info-get-method)

(def-info-func interface-info-find-method
  (interface-info info-ffi)
  (name :string))

(define-collection-getter interface-info-get-signals
    g-interface-info-get-n-signals g-interface-info-get-signal)

(def-info-func interface-info-find-signal
  (interface-info info-ffi)
  (name :string))

(define-collection-getter interface-info-get-vfuncs
    g-interface-info-get-n-vfuncs g-interface-info-get-vfunc)

(def-info-func interface-info-find-vfunc
  (interface-info info-ffi)
  (name :string))

(define-collection-getter interface-info-get-constants
    g-interface-info-get-n-constants g-interface-info-get-constant)

(def-info-func (interface-info-get-class-struct g-interface-info-get-iface-struct)
  (interface-info info-ffi))

;;;
;;; property-info
;;;

(cffi:defbitfield param-flags
    "aspects of parameters"
  :readable
  :writable
  :construct
  :construct-only
  :lax-validation
  :static-name
  :static-nick
  :static-blurb
  (:deprecated  #X80000000))

(cffi:defcfun (property-info-get-flags "g_property_info_get_flags") param-flags
  (property-info info-ffi))

(def-info-func property-info-get-type
  (property-info info-ffi))

;;;
;;; signal-info
;;;

(cffi:defbitfield signal-flags
    "signal's behaviour"
  :run-first
  :run-last
  :run-cleanup
  :no-recurse
  :detailed
  :action
  :no-hooks)

(cffi:defcfun (signal-info-get-flags "g_signal_info_get_flags") signal-flags
  (signal-info info-ffi))

(def-info-func signal-info-get-class-closure
  (signal-info info-ffi))

(cffi:defcfun (signal-info-true-stops-emit "g_signal_info_true_stops_emit") :boolean
  (signal-info info-ffi))

;;;
;;; vfunc-info
;;;

(cffi:defbitfield vfunc-info-flags
    "Flags of a vfunc-info struct."
  :must-chain-up
  :must-override
  :must-not-override)

(cffi:defcfun (vfunc-info-get-flags "g_vfunc_info_get_flags") vfunc-info-flags
  (vfunc-info info-ffi))

(cffi:defcfun (vfunc-info-get-offset "g_vfunc_info_get_offset") :int
  (vfunc-info info-ffi))

(def-info-func vfunc-info-get-signal
  (vfunc-info info-ffi))

(def-info-func vfunc-info-get-invoker
  (vfunc-info info-ffi))

;;;
;;; constant-info
;;;

(def-info-func constant-info-get-type
  (constant-info info-ffi))

(progn 
  (cffi:defcfun g-constant-info-get-value :int
    (constant-info info-ffi)
    (value (:pointer (:union argument))))
  
  (defun constant-info-get-value (constant-info)
    (cffi:with-foreign-object (value '(:pointer (:union argument)))
      (let ((giarg-type (gir::build-argument-type (constant-info-get-type constant-info) :nothing)))
	(g-constant-info-get-value constant-info value)
	(mem-get value giarg-type)))))

;;;
;;; function-info
;;;

(cffi:defbitfield function-info-flags
    ""
  :is-method
  :is-constructor
  :is-getter
  :is-setter
  :wraps-vfunc
  :throws)

(cffi:defcfun (function-info-get-symbol "g_function_info_get_symbol") :string
  (function-info info-ffi))

(cffi:defcfun (function-info-get-flags "g_function_info_get_flags") function-info-flags
  (function-info info-ffi))

(def-info-func function-info-get-property
  (function-info info-ffi))

(def-info-func function-info-get-vfunc
  (function-info info-ffi))

#|
#define G_INVOKE_ERROR (g_invoke_error_quark ())
GQuark g_invoke_error_quark (void);

typedef enum
{
  G_INVOKE_ERROR_FAILED,
  G_INVOKE_ERROR_SYMBOL_NOT_FOUND,
  G_INVOKE_ERROR_ARGUMENT_MISMATCH
} GInvokeError;

gboolean              g_function_info_invoke         (GIFunctionInfo *info,
						      const GArgument  *in_args,
						      int               n_in_args,
						      const GArgument  *out_args,
						      int               n_out_args,
						      GArgument        *return_value,
						      GError          **error);

|#

