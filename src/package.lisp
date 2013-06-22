;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:gir
  (:use #:common-lisp #:iterate)
  (:export 
   #:base-info
   #:type-info
   #:callable-info
   #:function-info
   #:callback-info
   #:signal-info
   #:vfunc-info
   #:error-domain-info
   #:value-info
   #:field-info
   #:registered-type-info
   #:enum-info
   #:interface-info
   #:object-info
   #:struct-info
   #:union-info
   #:property-info
   #:constant-info
   #:arg-info
   
   #:object-info-get-type-name 
   #:registered-type-info-get-type-init
   #:interface-info-get-prerequisites
   #:type-info-get-interface
   #:info-get-namespace
   #:type-info-get-array-fixed-size
   #:type-info-get-array-length
   #:type-tag-to-string #:field-info-get-size
   #:object-info-get-interfaces
   #:vfunc-info-get-signal
   #:repository-get-dependencies
   #:object-info-get-class-struct
   #:info-get-name #:typelib-symbol
   #:info-get-typelib
   #:function-info-get-property
   #:field-info-get-type
   #:arg-info-get-ownership-transfer
   #:vfunc-info-get-offset
   #:type-info-is-zero-terminated
   #:repository-is-registered
   #:repository-find-by-gtype
   #:union-info-get-discriminators
   #:object-info-get-type-init
   #:union-info-get-alignment
   #:callable-info-get-args
   #:arg-info-may-be-null
   #:repository-require #:repository-new
   #:field-info-get-offset
   #:info-get-attributes
   #:struct-info-get-fields
   #:repository-get-shared-library
   #:object-info-find-method
   #:struct-info-is-gtype-struct
   #:property-info-get-flags
   #:arg-info-is-optional
   #:property-info-get-type
   #:union-info-is-discriminated
   #:g-object-info-get-fields
   #:struct-info-find-method
   #:repository-get-version
   #:info-get-container
   #:object-info-get-abstract
   #:object-info-get-constants
   #:interface-info-find-method
   #:arg-info-get-destroy
   #:arg-info-is-caller-allocates
   #:type-info-is-pointer
   #:function-info-get-vfunc
   #:object-info-get-parent
   #:repository-prepend-search-path
   #:repository-load-typelib
   #:callable-info-may-return-null
   #:with-typelibs #:repository-get-c-prefix
   #:interface-info-get-signals
   #:enum-info-get-storage-type
   #:enum-info-get-values
   #:union-info-get-discriminator-type
   #:typelib-namespace
   #:interface-info-get-iface-struct
   #:type-info-get-param-type
   #:callable-info-get-return-type
   #:union-info-get-methods
   #:type-info-get-tag #:value-info-get-value
   #:object-info-get-signals
   #:repository-find-by-name
   #:object-info-find-vfunc
   #:struct-info-get-methods
   #:arg-info-get-scope
   #:vfunc-info-get-invoker #:with-typelib
   #:registered-type-info-get-type-name
   #:repository-get-infos
   #:interface-info-get-vfuncs #:typelib-new
   #:info-equal #:union-info-get-size
   #:interface-info-find-vfunc
   #:object-info-get-methods
   #:signal-info-get-class-closure
   #:repository-get-typelib-path
   #:repository-get-loaded-namespaces
   #:struct-info-get-size
   #:vfunc-info-get-flags
   #:arg-info-get-direction
   #:repository-get-search-path
   #:signal-info-true-stops-emit
   #:constant-info-get-value
   #:signal-info-get-flags
   #:arg-info-get-closure
   #:info-is-deprecated
   #:union-info-find-method
   #:callable-info-get-caller-owns
   #:info-unref #:function-info-get-flags
   #:field-info-get-flags #:typelib-free
   #:struct-info-is-foreign
   #:arg-info-is-return-value
   #:union-info-get-fields
   #:union-info-get-discriminator-offset
   #:constant-info-get-type
   #:repository-get-default
   #:function-info-get-symbol
   #:type-info-get-array-type
   #:object-info-get-vfuncs
   #:object-info-get-properties
   #:interface-info-get-properties
   #:registered-type-info-get-g-type
   #:interface-info-get-methods
   #:arg-info-get-type #:info-get-type
   #:struct-info-get-alignment
   #:interface-info-get-constants

   #:ffi
   #:call))

