(in-package :gir)

(defun struct-closures (info)
  (let* ((call (lambda (name args)
                 (let ((function-info (struct-info-find-method 
                                       info (c-name name))))
                   (if function-info
                       (apply (build-function function-info) args)
                       (error "Bad FFI method name ~a" name)))))
         (fields-dict
          (loop :for field-info :in (struct-info-get-fields info)
             :collect
             (cons (info-get-name field-info) field-info)))
         (find-field (lambda (name)
                       (cdr (or (assoc (c-name name) fields-dict)
                                (error "Bad FFI field name ~a" name)))))
         (closure (lambda (this)
                    (let ((signals (list nil)))
                      (lambda (name &rest args)
                        (case name
                          (:this this)
                          (:signals signals)
                          (:field
                           (destructuring-bind (name) args
                             (gir.field:get this (funcall find-field name))))
                          (:set-field!
                           (destructuring-bind (name value) args
                             (gir.field:set this 
                                            (funcall find-field name) 
                                            value)))
                          (t (funcall call name (cons this args)))))))))
    (values call closure)))

(defun build-struct (info)
  (multiple-value-bind (call closure) (struct-closures info)
    (lambda (name &rest args)
      (let ((this (if (cffi:pointerp name) name 
                      (funcall call name args))))
        (funcall closure this)))))

(defun build-struct-ptr (info ptr)
  (multiple-value-bind (call closure) (struct-closures info)
    (declare (ignore call))
    (funcall closure ptr)))