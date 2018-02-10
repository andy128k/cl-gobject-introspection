
;; (ql:quickload '("cffi" "cffi-objects"))

(in-package :cl-user)

(defpackage :glib.timeout-add
  (:use :cl :cffi :cffi-objects)
  (:export #:timeout-add
           #:timeout-remove))

(in-package :glib.timeout-add)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library :g-lib
    (:unix (:or "libglib-2.0.so.0" "libglib-2.0.so"))
    (:windows "libglib-2.0-0.dll"))

  (load-foreign-library :g-lib))

(defcfun "g_idle_add" :uint (func pfunction) (data :pointer))

(defcfun "g_idle_add_full" :uint (prio :int) (func pfunction)
  (data :pointer) (notify-handler pfunction))

(defcfun "g_timeout_add_full" :uint (prio :int)
  (interval :uint) (func pfunction) (data :pointer) (notify-handler pfunction))

(defcfun "g_timeout_add_seconds_full" :uint (prio :int)
  (interval :uint) (func pfunction) (data :pointer) (notify-handler pfunction))

(defcfun "g_timeout_add" :uint (interval :uint)
  (func pfunction) (data :pointer))

(defcfun "g_timeout_add_seconds" :uint (interval :uint)
  (func pfunction) (data :pointer))

(defvar *timeout-funcs* nil
  "Assoc array number -> function | (function data)")
(defvar *timeout-maxnum* 0
  "Next number for *timeout-funcs* adding")

(defun ptr-timeout (func data destroy-notify)
  (let ((next (incf *timeout-maxnum*)))
    (setf *timeout-funcs*
          (acons next
                 (list func data destroy-notify)
                 *timeout-funcs*))
    (make-pointer next)))

(defcallback timeout-func 
    :boolean ((data-ptr :pointer))
  (let ((func-list (cdr (assoc (pointer-address data-ptr)
                               *timeout-funcs*))))
    (destructuring-bind (func data notify) func-list
      (declare (ignore notify))
      (apply func data))))

(defcallback free-timeout 
    :void ((data :pointer))
  (let ((key (pointer-address data)))
    (let* ((func-list (cdr (assoc key *timeout-funcs*)))
           (notify (third func-list)))
      (when notify (funcall notify)))
    (setf *timeout-funcs* 
          (delete-if (lambda (x)
                       (eq key (car x)))
                     *timeout-funcs*))))

(defun timeout-add (interval func &key data priority destroy-notify)
  "Sets a function to be called at regular intervals.
 FUNC may be a lisp function or a foreign pointer to C function
 INTERVAL may be a number (= time in milliseconds), list
   (1 sec) (1 secs) (1 seconds) or (1 ms) or :idle
 DESTROY-NOTIFY should be the same type (function or pointer) as FUNC"
  (declare (type (or fixnum list keyword) interval)
           (type (or function foreign-pointer) func)
           (type (or null function foreign-pointer) destroy-notify)
           (type (or null fixnum) priority)
           (type list data))
  (if (eq interval :idle)
      (if (functionp func)
          (g-idle-add-full
           (or priority 200)
           (callback timeout-func)
           (ptr-timeout func data destroy-notify)
           (callback free-timeout))
          (if (and (null priority) (null destroy-notify))
              (g-idle-add func data)
              (g-idle-add-full
               (or priority 200) func data destroy-notify)))
      (let ((seconds (and (listp interval)
                          (find (second interval) 
                                '(sec secs second seconds s))))
            (%interval (if (listp interval) (car interval) interval))
            (%priority (or priority 0)))
        (if (functionp func)
            (funcall (if seconds #'g-timeout-add-seconds-full
                         #'g-timeout-add-full)
                     %priority %interval (callback timeout-func)
                     (ptr-timeout func data destroy-notify)
                     (callback free-timeout))
            (if (and (null priority) (null destroy-notify))
                (funcall (if seconds #'g-timeout-add-seconds
                             #'g-timeout-add) %interval func data)
                (funcall (if seconds #'g-timeout-add-seconds-full
                             #'g-timeout-add-full)
                         %priority %interval func data destroy-notify))))))

(defcfun ("g_source_remove" timeout-remove) :boolean (tag :uint))
