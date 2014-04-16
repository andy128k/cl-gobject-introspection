(in-package :gir)

(defun build-interface (info)
  (etypecase info
    (function-info (build-function info))
    (object-info (build-object info))
    (struct-info (build-struct info))
    (enum-info (build-enum info))
    (constant-info (constant-info-get-value info))))

(defun ffi (namespace &optional (version (cffi:null-pointer)))
  (repository-require nil namespace version)
  (lambda (name)
    (let ((info (repository-find-by-name nil namespace (c-name name))))
      (if info
          (build-interface info)
          (warn "No such FFI name ~a" name)))))

(defun nget (namespace &rest args)
  (dolist (arg args namespace)
    (setf namespace (funcall namespace arg))))

(defmacro invoke (func &rest args)
  (if (listp func)
  `(funcall (nget ,@func) ,@args)
  `(funcall ,func ,@args)))
