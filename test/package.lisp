;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage gir-test
  (:use :common-lisp :iterate :gir :fiveam :cffi)
  (:export :main))

(in-package :gir-test)

(defun read-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((v (make-array (list (file-length stream)) :element-type '(unsigned-byte 8))))
      (read-sequence v stream)
      v)))

