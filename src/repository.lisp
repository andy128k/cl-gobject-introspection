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
  (cffi:defcfun g-irepository-get-type :int32)
  (cffi:defcfun g-object-newv :pointer
    (g-type :int32)
    (n-params :int))
  
  (defun repository-new ()
    (make-repository :ptr
		     (g-object-newv (g-irepository-get-type) 0)))

  (export 'repository-new))


(cffi:defcfun (repository-get-default "g_irepository_get_default") repository-type)
(export 'repository-get-default)


(cffi:defcfun (repository-prepend-search-path "g_irepository_prepend_search_path") :void
  (directory :string))
(export 'repository-prepend-search-path)


(progn
  (cffi:defcfun g-irepository-get-search-path :pointer)
  
  (defun repository-get-search-path ()
    (g-slist-to-list (g-irepository-get-search-path)))
  
  (export 'repository-get-search-path))


(progn
  (cffi:defcfun g-irepository-load-typelib :string
    (repository repository-type)
    (typelib typelib-type)
    (flags load-flags)
    (gerror :pointer))
  
  (defun repository-load-typelib (repository typelib &optional flags)
    (with-gerror err
      (g-irepository-load-typelib repository typelib flags err)))
  
  (export 'repository-load-typelib))


(cffi:defcfun (repository-is-registered "g_irepository_is_registered") :boolean
  (repository repository-type)
  (namespace :string)
  (version :string))
(export 'repository-is-registered)


(cffi:defcfun (repository-find-by-name "g_irepository_find_by_name") info-ffi
  (repository repository-type)
  (namespace :string)
  (name :string))
(export 'repository-find-by-name)


(progn
  (cffi:defcfun g-irepository-require typelib-type
    (repository repository-type)
    (namespace :string)
    (version :string)
    (flags load-flags)
    (gerror :pointer))
  
  (defun repository-require (repository namespace version &optional flags)
    (with-gerror err (g-irepository-require repository namespace version flags err)))
  
  (export 'repository-require))


(cffi:defcfun (repository-get-dependencies "g_irepository_get_dependencies") strv-ffi
  (repository repository-type)
  (namespace :string))
(export 'repository-get-dependencies)


(cffi:defcfun (repository-get-loaded-namespaces "g_irepository_get_loaded_namespaces") strv-ffi
  (repository repository-type))
(export 'repository-get-loaded-namespaces)


(cffi:defcfun (repository-find-by-gtype "g_irepository_find_by_gtype") info-ffi
  (repository repository-type)
  (g-type :int))
(export 'repository-find-by-gtype)

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
	    (collect (g-irepository-get-info repository namespace i)))))

  (export 'repository-get-infos))

(cffi:defcfun (repository-get-typelib-path "g_irepository_get_typelib_path") :string
  (repository repository-type)
  (namespace :string))
(export 'repository-get-typelib-path)

(cffi:defcfun (repository-get-shared-library "g_irepository_get_shared_library") :string
  (repository repository-type)
  (namespace :string))
(export 'repository-get-shared-library)

(cffi:defcfun (repository-get-c-prefix "g_irepository_get_c_prefix") :string
  (repository repository-type)
  (namespace :string))
(export 'repository-get-c-prefix)

(cffi:defcfun (repository-get-version "g_irepository_get_version") :string
  (repository repository-type)
  (namespace :string))
(export 'repository-get-version)

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

(cffi:defcfun (callable-info-get-return-type "g_callable_info_get_return_type") info-ffi
  (callable-info info-ffi))
(export 'callable-info-get-return-type)

(cffi:defcenum transfer
    "Represent the transfer ownership information of a callable-info or a arg-info."
  :nothing
  :container
  :everything)

(cffi:defcfun (callable-info-get-caller-owns "g_callable_info_get_caller_owns") transfer
  (callable-info info-ffi))
(export 'callable-info-get-caller-owns)

(cffi:defcfun (callable-info-may-return-null "g_callable_info_may_return_null") :boolean
  (callable-info info-ffi))
(export 'callable-info-may-return-null)

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
(export 'arg-info-get-direction)

(cffi:defcfun (arg-info-is-return-value "g_arg_info_is_return_value") :boolean
  (arg-info info-ffi))
(export 'arg-info-is-return-value)

(cffi:defcfun (arg-info-is-optional "g_arg_info_is_optional") :boolean
  (arg-info info-ffi))
(export 'arg-info-is-optional)

(cffi:defcfun (arg-info-is-caller-allocates "g_arg_info_is_caller_allocates") :boolean
  (arg-info info-ffi))
(export 'arg-info-is-caller-allocates)

(cffi:defcfun (arg-info-may-be-null "g_arg_info_may_be_null") :boolean
  (arg-info info-ffi))
(export 'arg-info-may-be-null)

(cffi:defcfun (arg-info-get-ownership-transfer "g_arg_info_get_ownership_transfer") transfer
  (arg-info info-ffi))
(export 'arg-info-get-ownership-transfer)

(cffi:defcfun (arg-info-get-scope "g_arg_info_get_scope") scope-type
  (arg-info info-ffi))
(export 'arg-info-get-scope)

(cffi:defcfun (arg-info-get-closure "g_arg_info_get_closure") :int
  (arg-info info-ffi))
(export 'arg-info-get-closure)

(cffi:defcfun (arg-info-get-destroy "g_arg_info_get_destroy") :int
  (arg-info info-ffi))
(export 'arg-info-get-destroy)

(cffi:defcfun (arg-info-get-type "g_arg_info_get_type") info-ffi
  (arg-info info-ffi))
(export 'arg-info-get-type)

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
(export 'type-tag-to-string)

(cffi:defcfun (type-info-is-pointer "g_type_info_is_pointer") :boolean
  (type-info info-ffi))
(export 'type-info-is-pointer)

(cffi:defcfun (type-info-get-tag "g_type_info_get_tag") type-tag
  (type-info info-ffi))
(export 'type-info-get-tag)

(cffi:defcfun (type-info-get-param-type "g_type_info_get_param_type") info-ffi
  (type-info info-ffi)
  (n :int))
(export 'type-info-get-param-type)

(cffi:defcfun (type-info-get-interface "g_type_info_get_interface") info-ffi
  (type-info info-ffi))
(export 'type-info-get-interface)

(cffi:defcfun (type-info-get-array-length "g_type_info_get_array_length") :int
  (type-info info-ffi))
(export 'type-info-get-array-length)

(cffi:defcfun (type-info-get-array-fixed-size "g_type_info_get_array_fixed_size") :int
  (type-info info-ffi))
(export 'type-info-get-array-fixed-size)

(cffi:defcfun (type-info-is-zero-terminated "g_type_info_is_zero_terminated") :boolean
  (type-info info-ffi))
(export 'type-info-is-zero-terminated)

(cffi:defcfun (type-info-get-array-type "g_type_info_get_array_type") array-type
  (type-info info-ffi))
(export 'type-info-get-array-type)

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
(export 'value-info-get-value)

;;;
;;; field-info
;;;

(cffi:defbitfield field-info-flags
    "Flags for a field-info."
  :readable
  :writable)

(cffi:defcfun (field-info-get-flags "g_field_info_get_flags") field-info-flags
  (field-info info-ffi))
(export 'field-info-get-flags)

(cffi:defcfun (field-info-get-size "g_field_info_get_size") :int
  (field-info info-ffi))
(export 'field-info-get-size)

(cffi:defcfun (field-info-get-offset "g_field_info_get_offset") :int
  (field-info info-ffi))
(export 'field-info-get-offset)

(cffi:defcfun (field-info-get-type "g_field_info_get_type") info-ffi
  (field-info info-ffi))
(export 'field-info-get-type)

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
(export 'union-info-is-discriminated)

(cffi:defcfun (union-info-get-discriminator-offset "g_union_info_get_discriminator_offset") :int
  (union-info info-ffi))
(export 'union-info-get-discriminator-offset)

(cffi:defcfun (union-info-get-discriminator-type "g_union_info_get_discriminator_type") info-ffi
  (union-info info-ffi))
(export 'union-info-get-discriminator-type)

(define-collection-getter union-info-get-discriminators
    (g-union-info-get-n-fields :already-defined) g-union-info-get-discriminator)

(cffi:defcfun (union-info-find-method "g_union_info_find_method") info-ffi
  (union-info info-ffi)
  (name :string))
(export 'union-info-find-method)

(cffi:defcfun (union-info-get-size "g_union_info_get_size") :int
  (union-info info-ffi))
(export 'union-info-get-size)

(cffi:defcfun (union-info-get-alignment "g_union_info_get_alignment") :int
  (union-info info-ffi))
(export 'union-info-get-alignment)

;;;
;;; struct-info
;;;

(define-collection-getter struct-info-get-fields
    g-struct-info-get-n-fields g-struct-info-get-field)

(define-collection-getter struct-info-get-methods
    g-struct-info-get-n-methods g-struct-info-get-method)

(cffi:defcfun (struct-info-find-method "g_struct_info_find_method") info-ffi
  (struct-info info-ffi)
  (name :string))
(export 'struct-info-find-method)

(cffi:defcfun (struct-info-get-size "g_struct_info_get_size") :int
  (struct-info info-ffi))
(export 'struct-info-get-size)

(cffi:defcfun (struct-info-get-alignment "g_struct_info_get_alignment") :int
  (struct-info info-ffi))
(export 'struct-info-get-alignment)

(cffi:defcfun (struct-info-is-gtype-struct "g_struct_info_is_gtype_struct") :boolean
  (struct-info info-ffi))
(export 'struct-info-is-gtype-struct)

(cffi:defcfun (struct-info-is-foreign "g_struct_info_is_foreign") :boolean
  (struct-info info-ffi))
(export 'struct-info-is-foreign)

;;;
;;; registered-type-info
;;;

(cffi:defcfun (registered-type-info-get-type-name "g_registered_type_info_get_type_name") :string
  (registered-type-info info-ffi))
(export 'registered-type-info-get-type-name)

(cffi:defcfun (registered-type-info-get-type-init "g_registered_type_info_get_type_init") :string
  (registered-type-info info-ffi))
(export 'registered-type-info-get-type-init)

(cffi:defcfun (registered-type-info-get-g-type "g_registered_type_info_get_g_type") :int
  (registered-type-info info-ffi))
(export 'registered-type-info-get-g-type)

;;;
;;; enum-info
;;;

(define-collection-getter enum-info-get-values
    g-enum-info-get-n-values g-enum-info-get-value)

(cffi:defcfun (enum-info-get-storage-type "g_enum_info_get_storage_type") info-ffi
  (enum-info info-ffi))
(export 'enum-info-get-storage-type)

;;;
;;; object-info
;;;

(cffi:defcfun (object-info-get-type-name "g_object_info_get_type_name") :string
  (object-info info-ffi))
(export 'object-info-get-type-name)

(cffi:defcfun (object-info-get-type-init "g_object_info_get_type_init") :string
  (object-info info-ffi))
(export 'object-info-get-type-init)

(cffi:defcfun (object-info-get-abstract "g_object_info_get_abstract") :boolean
  (object-info info-ffi))
(export 'object-info-get-abstract)

(cffi:defcfun (object-info-get-parent "g_object_info_get_parent") info-ffi
  (object-info info-ffi))
(export 'object-info-get-parent)

(define-collection-getter object-info-get-interfaces
    g-object-info-get-n-interfaces g-object-info-get-interface)

(define-collection-getter g-object-info-get-fields
    g-object-info-get-n-fields g-object-info-get-field)

(define-collection-getter object-info-get-properties
    g-object-info-get-n-properties g-object-info-get-property)

(define-collection-getter object-info-get-methods
    g-object-info-get-n-methods g-object-info-get-method)

(cffi:defcfun (object-info-find-method "g_object_info_find_method") info-ffi
  (object-info info-ffi)
  (name :string))
(export 'object-info-find-method)

(define-collection-getter object-info-get-signals
    g-object-info-get-n-signals g-object-info-get-signal)

(define-collection-getter object-info-get-vfuncs
    g-object-info-get-n-vfuncs g-object-info-get-vfunc)

(cffi:defcfun (object-info-find-vfunc "g_object_info_find_vfunc") info-ffi
  (object-info info-ffi)
  (name :string))
(export 'object-info-find-vfunc)

(define-collection-getter object-info-get-constants
    g-object-info-get-n-constants g-object-info-get-constant)

(cffi:defcfun (object-info-get-class-struct "g_object_info_get_class_struct") info-ffi
  (object-info info-ffi))
(export 'object-info-get-class-struct)

;;;
;;; interface-info
;;;

(define-collection-getter interface-info-get-prerequisites
    g-interface-info-get-n-prerequisites g-interface-info-get-prerequisite)

(define-collection-getter interface-info-get-properties
    g-interface-info-get-n-properties g-interface-info-get-property)

(define-collection-getter interface-info-get-methods
    g-interface-info-get-n-methods g-interface-info-get-method)

(cffi:defcfun (interface-info-find-method "g_interface_info_find_method") info-ffi
  (interface-info info-ffi)
  (name :string))
(export 'interface-info-find-method)

(define-collection-getter interface-info-get-signals
    g-interface-info-get-n-signals g-interface-info-get-signal)

(define-collection-getter interface-info-get-vfuncs
    g-interface-info-get-n-vfuncs g-interface-info-get-vfunc)

(cffi:defcfun (interface-info-find-vfunc "g_interface_info_find_vfunc") info-ffi
  (interface-info info-ffi)
  (name :string))
(export 'interface-info-find-vfunc)

(define-collection-getter interface-info-get-constants
    g-interface-info-get-n-constants g-interface-info-get-constant)

(cffi:defcfun (interface-info-get-class-struct "g_interface_info_get_iface_struct") info-ffi
  (interface-info info-ffi))
(export 'interface-info-get-iface-struct)

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
(export 'property-info-get-flags)

(cffi:defcfun (property-info-get-type "g_property_info_get_type") info-ffi
  (property-info info-ffi))
(export 'property-info-get-type)

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
(export 'signal-info-get-flags)

(cffi:defcfun (signal-info-get-class-closure "g_signal_info_get_class_closure") info-ffi
  (signal-info info-ffi))
(export 'signal-info-get-class-closure)

(cffi:defcfun (signal-info-true-stops-emit "g_signal_info_true_stops_emit") :boolean
  (signal-info info-ffi))
(export 'signal-info-true-stops-emit)

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
(export 'vfunc-info-get-flags)

(cffi:defcfun (vfunc-info-get-offset "g_vfunc_info_get_offset") :int
  (vfunc-info info-ffi))
(export 'vfunc-info-get-offset)

(cffi:defcfun (vfunc-info-get-signal "g_vfunc_info_get_signal") info-ffi
  (vfunc-info info-ffi))
(export 'vfunc-info-get-signal)

(cffi:defcfun (vfunc-info-get-invoker "g_vfunc_info_get_invoker") info-ffi
  (vfunc-info info-ffi))
(export 'vfunc-info-get-invoker)

;;;
;;; constant-info
;;;

(cffi:defcfun (constant-info-get-type "g_constant_info_get_type") info-ffi
  (constant-info info-ffi))
(export 'constant-info-get-type)

(progn 
  (cffi:defcfun g-constant-info-get-value :int
    (constant-info info-ffi)
    (value (:pointer (:union argument))))
  
  (defun constant-info-get-value (constant-info)
    (cffi:with-foreign-object (value '(:pointer (:union argument)))
      (let ((length (g-constant-info-get-value constant-info value)))
	(argument->lisp-value value length (constant-info-get-type constant-info)))))
  
  (export 'constant-info-get-value))

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
(export 'function-info-get-symbol)

(cffi:defcfun (function-info-get-flags "g_function_info_get_flags") function-info-flags
  (function-info info-ffi))
(export 'function-info-get-flags)

(cffi:defcfun (function-info-get-property "g_function_info_get_property") info-ffi
  (function-info info-ffi))
(export 'function-info-get-property)

(cffi:defcfun (function-info-get-vfunc "g_function_info_get_vfunc") info-ffi
  (function-info info-ffi))
(export 'function-info-get-vfunc)

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

