
(in-package :maze-example)

(defun make-fully-walled-maze (width height)
  (let ((sn-walls (make-array (list (1+ width)
                                    height)
                              :initial-element t))
        (we-walls (make-array (list (1+ height)
                                    width)
                              :initial-element t))) 
    (make-maze :height height
               :width width
               :sn-walls sn-walls
               :we-walls we-walls)))

(defmacro do-xy-visiting-array ((array x y)
                                &body code)
  (let ((height-s (gensym "HEIGHT-"))
        (width-s (gensym "WIDTH-")))
    `(destructuring-bind (,width-s ,height-s)
         (array-dimensions ,array)
       (dotimes (,y ,height-s)
         (dotimes (,x ,width-s)
           ,@code)))))

(defun whole-maze-is-visited? (visiting-array)
  (block fun
    (do-xy-visiting-array (visiting-array x y)
      (unless (aref visiting-array x y)
        (return-from fun nil)))
    t))

(defun not-visited-neighbors (cell visiting-array)
  (let ((neighbors (list :north :east :south :west)))
    (destructuring-bind (width height)
        (array-dimensions visiting-array)
      (destructuring-bind (x . y)
          cell
        (flet (($delete-neighbor! (n)
                 (setf neighbors (delete n neighbors))))
          (macrolet (($check-direction! (direction
                                         border-limit-condition
                                         nx ny)
                       `(when (or ,border-limit-condition
                                  (aref visiting-array ,nx ,ny))
                          ($delete-neighbor! ,direction))))
            ($check-direction! :east (= x (- width 1))
                               (+ x 1) y)
            ($check-direction! :west (= x 0)
                               (- x 1) y)
            ($check-direction! :north (= y 0)
                               x (- y 1))
            ($check-direction! :south (= y (- height 1))
                               x (+ y 1))))))
    neighbors))

(defun dig-in! (maze cell direction)
  (destructuring-bind (x . y)
      cell
    (with-slots (sn-walls we-walls)
        maze
      (macrolet (($dig (&rest aref-args)
                   `(setf (aref ,@aref-args)
                          nil)))
        (ecase direction
          (:east
           ($dig sn-walls
                 (1+ x)
                 y)
           (values (1+ x)
                   y))
          (:west
           ($dig sn-walls x y)
           (values (- x 1)
                   y))
          (:north
           ($dig we-walls y x)
           (values x (- y 1)))
          (:south ($dig we-walls
                        (1+ y)
                        x)
           (values x (1+ y))))))))

(defun get-random-unvisited-cell (visiting-array)
  (macrolet (($do-each-unvisited-cell ((x y)
                                       &body code)
               `(do-xy-visiting-array (visiting-array ,x ,y)
                  (unless (aref visiting-array ,x ,y)
                    ,@code))))
    (let* ((lucky-num (let ((c 0))
                        ($do-each-unvisited-cell (x y)
                          (incf c)) 
                        (random c))))
      (block f
        (let ((i 0))
          ($do-each-unvisited-cell (x y)
            (if (= lucky-num i)
                (return-from f (values x y))
                (incf i))))))))

(defun generate-maze (width height
                          &aux (x 0) (y 0))
  (let* ((maze (make-fully-walled-maze width height)) 
         (visiting-array (make-array (list width height)
                                     :initial-element nil))
         (cell-stack (list))
         c)
    (flet (($visit-cell (x y)
             (setf (aref visiting-array x y)
                   t
                   c (cons x y))))
      ($visit-cell x y)
      (loop :until (whole-maze-is-visited? visiting-array) 
            :do (let ((nlist (not-visited-neighbors c visiting-array)))
                  (cond
                    (nlist
                     (push c cell-stack)
                     (let ((dir (nth (random (length nlist))
                                     nlist))) 
                       (multiple-value-bind (x y)
                           (dig-in! maze c dir) 
                         ($visit-cell x y))))
                    (cell-stack
                     (setf c (pop cell-stack)))
                    (t
                     (multiple-value-bind (x y)
                         (get-random-unvisited-cell visiting-array) 
                       ($visit-cell x y)))))
            :finally (return maze)))))
