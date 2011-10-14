;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage gir
  (:use :common-lisp :iterate)
  (:export 
   :base-info
   :type-info
   :callable-info
   :function-info
   :callback-info
   :signal-info
   :vfunc-info
   :error-domain-info
   :value-info
   :field-info
   :registered-type-info
   :enum-info
   :interface-info
   :object-info
   :struct-info
   :union-info
   :property-info
   :constant-info
   :arg-info))

