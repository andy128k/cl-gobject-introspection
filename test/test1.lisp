;; -*- mode: Common-Lisp -*-

(in-package :gir-test)

(defun load-typelib (file-name)
  (iter (for dir-path in (gir:repository-get-search-path))
	(let* ((file-path (format nil "~a/~a" dir-path file-name))
	       (content (read-file file-path)))
	  (when content
	    (return content)))))

#+nil
(progn
  (format t "default: ~A~%" (gir:repository-get-default))
  (format t "search paths: ~A~%" (gir:repository-get-search-path))

  (let ((file (load-typelib "Pango-1.0.typelib")))
    (with-typelib tlb file 
      
      (format t "namespace: ~A~%" (typelib-namespace tlb))
      (format t "pango_version_string: ~A~%" (typelib-symbol tlb "pango_version_string"))
      
      (let ((repo (repository-new)))
	(format t "typelib: ~A~%" (repository-load-typelib repo tlb :lazy))
	
	(let ((i (repository-find-by-name repo "Pango" "Context")))
	  (format t "Context's type: ~A~%" (info-get-type i))
	  (format t "Context's name: ~A~%" (info-get-name i))
	  (format t "Context's namespace: ~A~%" (info-get-namespace i))
	  (format t "Context's attributes: ~A~%" (info-get-attributes i))
	  (format t "Context's container: ~A~%" (info-get-container i))
	  (format t "Context's typelib: ~A~%" (info-get-typelib i))
	  )

	(let ((i (repository-find-by-name repo "Pango" "version_string")))
	  (format t "version_string's args: ~A~%" (callable-info-get-args i))
	  )

	(format t "dependencies: ~A~%" (repository-get-dependencies repo "Pango"))
	(format t "namespaces: ~A~%" (repository-get-loaded-namespaces repo))
	(format t "infos: ~A~%" (repository-get-infos repo "Pango"))

	))))

