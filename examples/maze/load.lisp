
(in-package :cl-user)

(ql:quickload '(;; "cffi"
                ;; "cffi-objects"
                "cl-gobject-introspection"

                ;; Accessible Cairo API through gobject introspection
                ;; is quite poor
                "cl-cairo2"))

(let ((*default-pathname-defaults* 
        (merge-pathnames #P"examples/maze/src/"
                         (asdf:system-source-directory "cl-gobject-introspection"))))
  (load #P"./package.lisp") 
  (load #P"./maze.lisp")
  (load #P"./maze-generator.lisp")
  (load #P"./gui/drawing.lisp")
  (load #P"./gui/edit.lisp")
  (load #P"./gui/walking-through-maze.lisp"))

(in-package :maze-example)

(setf *random-state*
      (make-random-state t))

(defparameter *maze*
  (generate-maze 10 8))

(edit-maze-in-window! *maze*)


(walk-through-maze *maze*)
;; Shortcuts:
;; C-q  -  close window
;; H    -  hide
;; arrows for moving
