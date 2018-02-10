
(in-package :cl-user)

(ql:quickload '("cffi"
                "cffi-objects"
                "cl-gobject-introspection"

                ;; Accessible Cairo API through gobject introspection
                ;; is quite poor
                "cl-cairo2"))

(let ((*default-pathname-defaults*
        (merge-pathnames #P"examples/flood-game/"
                         (asdf:system-source-directory "cl-gobject-introspection"))))

  ;; It seems GLib timeouts is not working through gobject introspection
  ;; so this example requires this:
  (load #P"./src/glib-timeout-add.lisp")
  ;; ... with some low-level API from cffi and cffi-objects libraries

  (load #P"./src/package.lisp")
  (load #P"./src/gui.lisp")
  (load #P"./src/flood-game.lisp"))

(in-package :flood-game-example)

(defparameter *image*
  (copy-image #2A((0 3 0 2 0 3 1 3 2 0 3 0)
                  (2 3 0 2 3 1 3 0 2 0 2 1)
                  (0 2 3 3 2 1 2 3 3 0 2 0)
                  (2 1 1 0 1 1 3 2 1 2 0 0)
                  (1 2 2 1 2 2 3 0 1 0 0 1)
                  (0 0 2 2 3 1 2 2 3 3 3 1)
                  (2 0 1 3 0 2 0 1 0 3 1 3)
                  (3 1 2 3 1 3 2 0 2 3 3 1)
                  (2 0 3 2 0 1 2 0 3 3 0 2)
                  (2 2 1 0 2 2 3 0 1 0 2 3)
                  (3 3 2 1 0 0 0 2 2 0 1 1)
                  (1 1 2 3 3 2 2 1 0 3 1 2))))

;; (gtk-window-with-cairo-painting 'render-image-in-cairo-context)

(visualize-solution-in-gtk-window
 '(3 2 1 2 0 1 3 2 3 0 2 1 0 3 2))
