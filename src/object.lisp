(in-package :gir)

(defun c-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (substitute #\_ #\- (symbol-name name))))))

(defun find-ffi-method (info name)
  (and info
       (or (object-info-find-method info name)
           (find-ffi-method (object-info-get-parent info) name))))

(defun name-this? (name)
  (eq name :this))

(defun build-object (info)
  (flet ((call (name args)
              (let ((function-info (find-ffi-method info (c-name name))))
                (if function-info
                    (apply (build-function function-info) args)
                    (error "Should be FFI method name: ~a" name)))))
    (lambda (name &rest args)
      (let ((this (call name args)))
        (lambda (name &rest args)
          (if (name-this? name)
              this
              (call name (cons this args))))))))