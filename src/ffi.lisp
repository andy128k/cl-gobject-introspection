(in-package :gir)

(defmacro ensure-gethash-unless-null (key hash-table default &optional on-nil)
  (with-gensyms (value nvalue vkey vhash-table)
    `(let ((,vkey ,key)
	   (,vhash-table ,hash-table))
       (if-let ((,value (gethash ,vkey ,vhash-table)))
	 (values ,value t)
	 (values (if-let ((,nvalue ,default))
		   (setf (gethash ,vkey ,vhash-table) ,nvalue)
		   (progn ,on-nil nil))
		 nil)))))

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

(defclass namespace ()
  ((name :initarg :name :reader name-of)
   (version :reader version-of)
   (interface-cache :initform (make-hash-table :test 'equal)
		    :reader cache-of)))

(defmethod print-object ((ns namespace) s)
  (format s "#N<~a(~a)>" (name-of ns) (version-of ns)))

(defmethod shared-initialize :after ((namespace namespace) slot-names
				     &key name version)
  (declare (ignore slot-names))
  (repository-require nil name (if version version (cffi:null-pointer)))
  (setf (slot-value namespace 'version)
	(repository-get-version nil name)))

(defmethod nsget ((namespace namespace) name)
   (let ((cname (c-name name)))
     (ensure-gethash-unless-null cname (cache-of namespace)
				 (if-let ((info (repository-find-by-name nil (name-of namespace) cname)))
				   (build-interface info))
				 (warn "No such FFI name ~a" name))))

(defvar *namespace-cache* (make-hash-table :test 'equal))

(defun require-namespace (namespace &optional version)
  (let ((cache (gethash namespace *namespace-cache*)))
    (if (and cache (or (null version)
		       (equal version (version-of cache))))
	cache
	(setf (gethash namespace *namespace-cache*)
	      (make-instance 'namespace :name namespace :version version)))))

(declaim (inline ffi))
(defun ffi (namespace &optional version)
  (require-namespace namespace version))

(defun find-build-interface-for-name (nsname name)
  (let ((namespace (require-namespace nsname)))
      (nsget namespace name)))

(defun find-build-interface (info)
  (let* ((nsname (info-get-namespace info))
	 (namespace (require-namespace nsname)))
    (nsget namespace (info-get-name info))))
