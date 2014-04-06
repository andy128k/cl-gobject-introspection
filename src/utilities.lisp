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

(defun zero-memory (position length)
  (loop
     :for i :below length
     :for pos = position :then (cffi:inc-pointer pos 1)
     :do (setf (cffi:mem-ref pos :uint8) 0)))

(defun copy-memory (to from length)
  (loop
     :for i :below length
     :for pos-to = to :then (cffi:inc-pointer pos-to 1)
     :for pos-from = from :then (cffi:inc-pointer pos-from 1)
     :do (setf (cffi:mem-ref pos-to :uint8) (cffi:mem-ref pos-from :uint8))))

(defun zero? (position length)
  (loop
     :for i :below length
     :for pos = position :then (cffi:inc-pointer pos 1)
     :unless (eql (cffi:mem-ref pos :uint8) 0)
     :do (return-from zero? nil))
  t)
