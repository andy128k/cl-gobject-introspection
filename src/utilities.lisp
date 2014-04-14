(in-package :gir)

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

(defun allocate-finalize (pointer)
  (let ((addr (cffi:pointer-address pointer)))
    (tg:finalize pointer (lambda ()
			   (cffi:foreign-free (cffi:make-pointer addr)))))
  pointer)
