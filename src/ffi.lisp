(in-package :gir)

(defgeneric nsget (namespace name))

(defun nget (namespace &rest names)
  (dolist (name names namespace)
    (setf namespace (nsget namespace name))))

(defmacro invoke (func &rest args)
  (if (listp func)
  `(funcall (nget ,@func) ,@args)
  `(funcall ,func ,@args)))

(defmethod nsget ((namespace function) name)
  (funcall namespace name))

(defun build-interface (info)
  (etypecase info
    (function-info (build-function info))
    (object-info (build-object info))
    (struct-info (build-struct info))
    (enum-info (build-enum info))
    (constant-info (constant-info-get-value info))))

(defstruct
    (namespace
      (:constructor make-namespace (name version)))
  name
  version)

(defvar *namespace-cache* (make-hash-table :test 'equal))

(defun build-ffi (namespace &optional version)
  (repository-require nil namespace (if version version (cffi:null-pointer)))
  (make-namespace namespace version))

(defun ffi (namespace &optional version)
  (let ((cache (gethash namespace *namespace-cache*)))
    (if (and cache (or (null version)
		       (equal version (namespace-version cache))))
	cache
	(setf (gethash namespace *namespace-cache*)
	      (build-ffi namespace version)))))

(defmethod nsget ((namespace namespace) name)
   (let* ((nsname (namespace-name namespace))
	  (info (repository-find-by-name nil nsname (c-name name))))
     (if info
	 (build-interface info)
	 (warn "No such FFI name ~a" name))))
