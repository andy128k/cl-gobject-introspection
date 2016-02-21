(in-package :gir)

(defgeneric nsget (namespace name))
(defgeneric build-interface (info))

(defun nget (namespace &rest names)
  (dolist (name names namespace)
    (setf namespace (nsget namespace name))))

(defmacro invoke (func &rest args)
  (if (listp func)
      `(funcall (nget ,@func) ,@args)
      `(funcall ,func ,@args)))

(defmethod nsget ((namespace function) name)
  (funcall namespace name))

(defmethod build-interface ((info constant-info))
  (constant-info-get-value info))
(defstruct
    (namespace
      (:constructor make-namespace (name version interface-cache)))
  name
  version
  interface-cache)

(defvar *namespace-cache* (make-hash-table :test 'equal))

(defun build-ffi (namespace &optional version)
  (repository-require nil namespace (if version version (cffi:null-pointer)))
  (make-namespace namespace version (make-hash-table :test 'equal)))

(defun ffi (namespace &optional version)
  (let ((cache (gethash namespace *namespace-cache*)))
    (if (and cache (or (null version)
		       (equal version (namespace-version cache))))
	cache
	(setf (gethash namespace *namespace-cache*)
	      (build-ffi namespace version)))))

(declaim (inline require-namespace))
(defun require-namespace (namespace &optional version)
  (ffi namespace version))

(defmethod nsget ((namespace namespace) name)
   (let* ((nsname (namespace-name namespace))
	  (interface-cache (namespace-interface-cache namespace))
	  (cname (c-name name))
	  (interface (gethash cname interface-cache)))
     (if interface
	 interface
	 (let ((info (repository-find-by-name nil nsname cname)))
	   (if info
	       (setf (gethash cname interface-cache) (build-interface info))
	       (warn "No such FFI name ~a" name))))))

(defun find-build-interface-for-name (nsname name)
  (let ((namespace (require-namespace nsname)))
      (nsget namespace name)))

(defun find-build-interface (info)
  (let* ((namespace (info-get-namespace info))
	 (ffi (ffi namespace)))
    (nsget ffi (info-get-name info))))
