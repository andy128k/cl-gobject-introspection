(in-package :gir)

(defun get-const (info)
  (let ((giarg-res (make-giarg)))
    (constant-info-get-value info giarg-res)
    (make-out (build-translator (constant-info-get-type info)) giarg-res)))