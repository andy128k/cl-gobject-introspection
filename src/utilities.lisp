(in-package :gir)

(defun argv-alloc (args)
  (let* ((argc (length args))
	 (argv (cffi:foreign-alloc :pointer :count argc)))
    (loop
       :for i :below argc
       :for arg in args
       :do (setf (cffi:mem-aref argv :pointer i)
		 (cffi:foreign-string-alloc arg)))
    argv))

(defun argv-to-list (argc argv)
  (loop
     :for i :below argc
     :collect (cffi:foreign-string-to-lisp (cffi:mem-aref argv :pointer i))))
