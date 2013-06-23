(in-package :gir)

(cffi:defcfun g-value-info-get-value :int64 (info info-ffi))

(defun build-enum (info)
  (let ((values-dict
         (iter (for i below (g-enum-info-get-n-values info))
               (let ((value-info (g-enum-info-get-value info i)))
                 (collect (cons (info-get-name value-info)
                                (g-value-info-get-value value-info))))))
        (methods-dict 
         (iter (for i below (g-enum-info-get-n-methods info))               
               (let ((func-info (g-enum-info-get-method info i)))
                 (collect (cons (info-get-name func-info)
                                (build-function func-info)))))))
    (lambda (name &rest args)
      (let ((name* (c-name name)))
        (if (keywordp name)
            (cdr (or (assoc name* values-dict :test #'string=)
                     (error "Should be FFI enum value name: ~a" name)))
            (apply (cdr (or (assoc name* methods-dict :test #'string=)
                            (error "Should be FFI method name: ~a" name)))
                   args))))))