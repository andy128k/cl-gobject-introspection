(defpackage #:gir.field
  (:use #:cl)
  (:export #:get #:set)
  (:shadow #:get #:set))

(in-package #:gir.field)

(cffi:defcfun g-field-info-get-field 
  :boolean (field gir::info-ffi) (obj :pointer) (value :pointer))
(cffi:defcfun g-field-info-set-field 
  :boolean (field gir::info-ffi) (obj :pointer) (value :pointer))

(defun get (ptr field)
  (cffi:with-foreign-object (giarg '(:union gir:argument))
    (unless (g-field-info-get-field field ptr giarg)
      (error "FFI get field failed: ~a" (gir:info-get-name field)))
    (let ((giarg-type (gir::build-argument-type (gir:field-info-get-type field) :nothing)))
      (gir::mem-get giarg giarg-type))))

(defun set (ptr field value)
  (cffi:with-foreign-object (giarg '(:union gir:argument))
    (let ((giarg-type (gir::build-argument-type (gir:field-info-get-type field) :nothing)))
      (gir::mem-set giarg value giarg-type)
      (unless (g-field-info-set-field field ptr giarg)
	(error "FFI set field failed: ~a" (gir:info-get-name field))))))
