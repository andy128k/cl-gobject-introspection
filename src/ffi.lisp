(in-package :gir)

(defun build-interface (info args)
  (etypecase info
    (function-info (apply (build-function info) args))
    (object-info (apply (build-object info) args))
    (struct-info (apply (build-struct info) args))
    (enum-info (apply (build-enum info) args))
    (constant-info (constant-info-get-value info))))

(defun ffi (namespace &optional (version (cffi:null-pointer)))
  (repository-require nil namespace version)
  (lambda (name &rest rest)
    (let ((info (repository-find-by-name nil namespace (c-name name))))
      (if info
          (build-interface info rest)
          (warn "No such FFI name ~a" name)))))

(defun call (object &rest args)
  (apply object args))
      