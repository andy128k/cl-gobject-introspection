
(in-package :flood-game-example)


(defun make-image (&optional
                     (w 12)
                     (h w))
  (make-array (list w h)
              :element-type 'integer
              :initial-element 0))

(defvar *image* (make-image))

(defparameter *start-point*
  '(0 0))

(defun copy-image (image)
  (destructuring-bind (w h)
      (array-dimensions image)
    (let ((new-image (make-image w h)))
      (dotimes (i w)
        (dotimes (j h)
          (setf (aref new-image i j)
                (aref image i j))))
      new-image)))

(defun cell-neighbours (x y w h)
  (let ((neighbours (list)))
    (macrolet
        (($c (&rest clist)
           `(progn
              ,@(mapcar (lambda (form)
                          (destructuring-bind (exclusion &rest coords)
                              form
                            `(unless ,exclusion
                               (push (list ,@coords)
                                     neighbours))))
                        clist))))
      ($c ((zerop x)     (1- x) y)
          ((zerop y)     x      (1- y))
          ((= x (- w 1)) (1+ x) y)
          ((= y (- h 1)) x     (1+ y))))
    neighbours))

(defun fill-with-color! (color)
  (let ((cells-under-control-count 0)
        (initial-color (apply #'aref
                              *image*
                              *start-point*)))
    (values
     *image*
     (unless (eql color initial-color)
       (destructuring-bind (w h)
           (array-dimensions *image*)
         (let ((filling-points (list *start-point*)))
           (labels (($filling-neighbours (start-point)
                      (destructuring-bind (x y)
                          start-point
                        (loop :for p :in (cell-neighbours x y w h)
                              :when (= initial-color
                                       (apply #'aref *image* p))
                                :collect p)))
                    ($fill-from-point! (p)
                      (destructuring-bind (x y)
                          p
                        (unless (= (aref *image* x y)
                                   color)
                          (setf (aref *image* x y)
                                color)
                          (incf cells-under-control-count)
                          (let ((pl ($filling-neighbours p)))
                            (when pl
                              (destructuring-bind (p1 . rp)
                                  pl
                                (setf filling-points
                                      (nconc filling-points
                                             rp))
                                ($fill-from-point! p1))))))))
             (loop :while filling-points
                   :do ($fill-from-point! (pop filling-points))))))
       t)
     cells-under-control-count)))

(defun cairo-set-color (color)
  (apply #'cairo:set-source-rgba
         (append (case color
                   (0 '(1 0 0))
                   (1 '(139/255 0 1))
                   (2 '(13/255 150/255 29/255))
                   (3 '(1 220/255 16/255)))
                 (list 1))))

(defun render-image-in-cairo-context (c)
  (multiple-value-bind (x0 y0 csize nx ny)
      (destructuring-bind (w h)
          (array-dimensions *image*)
        (let* ((ch  (cairo:height c))
               (cw (cairo:width c))
               (csize (min (/ ch h)
                           (/ cw w))))
          (values (/ (- cw
                        (* csize w))
                     2)
                  (/ (- ch
                        (* csize h))
                     2)
                  csize
                  w h)))
    (cairo:with-context (c)
      (flet (($draw-cell (x y color)
               (cairo:rectangle x y csize csize)
               (cairo-set-color color) 
               (cairo:fill-path)))
        (dotimes (j ny)
          (let ((y (+ y0 (* j csize))))
            (dotimes (i nx)
              ($draw-cell (+ x0 (* i csize))
                          y
                          (aref *image* i j)))))
        (cairo:stroke)))))

(defvar *visualizing-solve-image*)
(defvar *visualizing-solution*
  nil)
(defvar *visualizing-solution-iteration*
  nil)

(defun visualize-solution-in-cairo-context (c)
  (if *visualizing-solution-iteration*
      (let ((*image* *visualizing-solve-image*))
        (destructuring-bind (t1 . rt)
            *visualizing-solution-iteration*
          (fill-with-color! t1)
          (setf *visualizing-solution-iteration* rt))
        (render-image-in-cairo-context c))
      (progn
        (setf *visualizing-solution-iteration*
              (copy-seq *visualizing-solution*)
              *visualizing-solve-image* (copy-image *image*))
        (render-image-in-cairo-context c))))

(defun visualize-solution-in-gtk-window (solution
                                         &optional (image *image*))
  (let ((*image* image)
        (*visualizing-solution* solution)
        (*visualizing-solution-iteration* nil))
    (gtk-window-with-cairo-painting
     'visualize-solution-in-cairo-context)))

