(in-package :gir)

(defvar *trampolines* (make-hash-table))

(defun make-trampoline (func &optional pointer)
  (let ((ptr (or pointer (cffi:foreign-alloc :int))))
    (setf (gethash (cffi:pointer-address ptr) *trampolines*) func)
    ptr))

(defun trampoline-get-function (ptr)
  (gethash (cffi:pointer-address ptr) *trampolines*))

(defun destroy-trampoline (ptr)
  (when (not (cffi:null-pointer-p ptr))
    (remhash (cffi:pointer-address ptr) *trampolines*)))

(cffi:defcallback destroy-trampoline :void ((ptr :pointer))
  (destroy-trampoline ptr))
