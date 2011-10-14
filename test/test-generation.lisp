;; -*- mode: Common-Lisp -*-

(in-package :gir-test)

(defun lispify-class (name &optional package)
  (let ((str (string-upcase
	      (with-output-to-string (stream)
		(iter (for c in-string name)
		      (when (and (not (first-iteration-p)) (upper-case-p c))
			(write-char #\- stream))
		      (write-char (char-upcase c) stream))))))
    (if package
	(intern str package)
	(intern str))))

(defun lispify (name &optional package)
  (let ((str (string-upcase
	      (with-output-to-string (stream)
		(iter (for c in-string name)
		      (write-char
		       (if (char= #\_ c)
			   #\-
			   (char-upcase c))
		       stream))))))
    (if package
	(intern str package)
	(intern str))))

(defun type-info->cffi-type (type)
  (case (type-info-get-tag type)
    (:void     :void)
    (:boolean  :boolean)
    (:int8     :int8)
    (:uint8    :uint8)
    (:int16    :int16)
    (:uint16   :uint16)
    (:int32    :int32)
    (:uint32   :uint32)
    (:int64    :int64)
    (:uint64   :uint64)
    (:float    :float)
    (:double   :double)
    (:gtype    :ulong)
    (:utf8     :string)
    (:filename :string)
    #|
    (:array (values (cffi:foreign-slot-value argument 'argument 'v-pointer)
                    length))
    |#
    (:interface 'g-interface)
    #|
    (:glist nil)
    (:gslist nil)
    (:ghash nil)
    (:error nil)
    |#
    (:unichar :uint32)
    (otherwise (error "TODO ~A" (type-info-get-tag type)))))

(defgeneric pp-info (info &key))

(defmethod pp-info ((info base-info) &key)
  info)

(defmethod pp-info ((info function-info) &key)
  (let ((inner-function (lispify (function-info-get-symbol info)))
	(outer-function (lispify (info-get-name info)))
	(throws (member :throws (function-info-get-flags info))))
    (labels ((error-wrap (invokation)
	       `(cffi:with-foreign-object (err :pointer)
		  (setf (cffi:mem-ref err :pointer) (cffi:null-pointer))
		  (prog1
		      ,(append invokation 'err)
		    (unless (cffi:null-pointer-p (cffi:mem-ref err :pointer))
		      (error "~A" (cffi:foreign-slot-value (cffi:mem-ref err :pointer) 'g-error 'message)))))))
      `(progn
	 (cffi::defcfun (,inner-function
			 ,(function-info-get-symbol info))
	     ,(type-info->cffi-type (callable-info-get-return-type info))
	   ,@(iter (for arg in (callable-info-get-args info))
		   (collect
		    (list (lispify (info-get-name arg))
			  (type-info->cffi-type (arg-info-get-type arg)))))
	   ,@(when throws
		   (list (list 'error 'g-error))))
	 
	 (defun ,outer-function
	     ,(iter (for arg in (callable-info-get-args info))
		    (when (member (arg-info-get-direction arg) '(:in :in-out))
		      (collect
		       (lispify (info-get-name arg)))))
	   ,(let* ((simple-invokation `(,inner-function
					,@(iter (for arg in (callable-info-get-args info))
						(collect
						 (lispify (info-get-name arg))))))
		   (out-args (iter (for arg in (callable-info-get-args info))
				   (when (eq (arg-info-get-direction arg) :out)
				     (collect arg))))
		   
		   (invokation (if out-args
				   `(cffi:with-foreign-objects
					,(iter (for arg in out-args)
					       (collect
						(list (lispify (info-get-name arg))
						      (type-info->cffi-type (arg-info-get-type arg)))))
				      (let ((result ,simple-invokation))
					(values result
						,@(iter (for arg in out-args)
							(collect (lispify (info-get-name arg)))))))
				   simple-invokation)))
		  
		  (if throws
		      (error-wrap invokation)
		      invokation)))
	 (export ',outer-function)))))

(defmethod pp-info ((info field-info) &key)
  (info-get-name info))

(defmethod pp-info ((info object-info) &key)
  `(define-g-object-class ,(object-info-get-type-name info) ,(lispify-class (object-info-get-type-name info))
     (:superclass ,(lispify-class (object-info-get-type-name (object-info-get-parent info)))
      :export t
      :type-initializer ,(registered-type-info-get-type-init info)
      :interfaces ,(mapcar #'info-get-name (object-info-get-interfaces info))
      (,@(object-info-get-properties info)))))

(defmethod pp-info ((info struct-info) &key)
  (unless (struct-info-is-gtype-struct info)
    `(defstruct ,(info-get-name info)
       ,@(iter (for m in (struct-info-get-methods info))
	       (collect (info-get-name m))))))

; (defmethod pp-info ((info function-info) &key)
;   (list "function" (info-get-name info)))

(defmethod pp-info ((info enum-info) &key)
  `(define-g-enum ,(registered-type-info-get-type-name info) ,(lispify-class (info-get-name info))
     (:export t :type-initializer ,(registered-type-info-get-type-init info))
     ,@(iter (for v in (enum-info-get-values info))
	     (collect (list
		       (lispify (info-get-name v) :keyword)
		       (value-info-get-value v))))))

(defmethod pp-info ((info constant-info) &key)
  `(defconstant ,(lispify (info-get-name info))
     ,(constant-info-get-value info)))

;; property
;; (do-overwrite-confirmation file-chooser-do-overwrite-confirmation "do-overwrite-confirmation" "gboolean" t t)

(repository-prepend-search-path "/usr/lib/girepository-1.0/")
(let ((repo (repository-get-default)))

  (repository-require repo "GIRepository" "2.0")
  ;; (repository-require repo "Pango" "1.0")
  ;; (repository-require repo "cairo" "1.0")
  ;; (repository-require repo "Gtk" "2.0")

  ;; (pprint (repository-get-dependencies repo "Gtk"))

  (iter (for info in (repository-get-infos repo "GIRepository"))
	;; (print info)
	(for pp = (pp-info info))
	(when pp
	  (pprint pp))
	))

