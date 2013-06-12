(in-package :gir)

(defun gtk-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (substitute #\_ #\- (symbol-name name))))))

(defun build-interface (info args)
  (etypecase info
    (function-info (apply (build-function info) args))))

(defun ffi (namespace &optional (version (cffi:null-pointer)))
  (repository-require nil namespace version)
  (lambda (name &rest rest)
    (let ((info (repository-find-by-name nil namespace (gtk-name name))))
      (if info
          (build-interface info rest)
          (warn "No such FFI name ~a" name)))))

(defun call (object &rest args)
  (apply object args))
      