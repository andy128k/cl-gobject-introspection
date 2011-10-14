;; -*- mode: Common-Lisp -*-

(in-package :gir-test)

#+nil
(progn
  (pprint (gir:repository-get-default))
  (pprint (gir:repository-get-search-path))
  
  (let ((file (read-file "/usr/lib/girepository-1.0/Pango-1.0.typelib")))
    (with-typelib
	tlb file 
	
	(pprint (typelib-namespace tlb))
	(pprint (typelib-symbol tlb "pango_version_string"))
	
	(let ((repo (repository-new)))
	  (pprint (repository-load-typelib repo tlb :lazy))
	  
	  (let ((i (repository-find-by-name repo "Pango" "Context")))
	    (pprint (info-get-type i))
	    (pprint (info-get-name i))
	    (pprint (info-get-namespace i))
	    (pprint (info-get-attributes i))
	    (pprint (info-get-container i))
	    (pprint (info-get-typelib i))
	    )

	  (let ((i (repository-find-by-name repo "Pango" "version_string")))
	    (pprint (callable-info-get-args i))
	    )

	  (pprint (repository-get-dependencies repo "Pango"))
	  (pprint (repository-get-loaded-namespaces repo))
	  (pprint (repository-get-infos repo "Pango"))

	  ))))

