
(in-package :maze-example)

(defun check-vwall (v n maze-geometry)
  (with-slots (nw nh)
      maze-geometry
    (when (and (<= 0 v nw)
               (<= 0 n (- nh 1)))
      (list :vwall v n))))

(defun check-hwall (h n maze-geometry)
  (with-slots (nw nh)
      maze-geometry
    (when (and (<= 0 h nh)
               (<= 0 n (- nw 1)))
      (list :hwall h n))))

(let ((d 0.15))
  (defun clickpoint->wallid (x y maze-geometry)
    (multiple-value-bind (xif yif)
        (with-slots (a (x0 x) (y0 y))
            maze-geometry 
          (values (/ (- x x0)
                     a)
                  (/ (- y y0)
                     a)))
      (multiple-value-bind (xi dx)
          (round xif)
        (multiple-value-bind (yi dy)
            (round yif)
          (let ((a (< (abs dx) d))
                (b (< (abs dy) d)))
            (macrolet (($hwall ()
                         `(check-hwall yi
                                       (floor xif)
                                       maze-geometry))
                       ($vwall ()
                         `(check-vwall xi
                                       (floor yif)
                                       maze-geometry)))
              (cond
                ((and a (not b))
                 ($vwall))
                ((and (not a) b)
                 ($hwall))
                ((and a b)
                 (if (eq (> dy dx)
                         (> dy (- dx)))
                     ($vwall)
                     ($hwall)))))))))))

(defun edit-wall (wallid maze)
  (destructuring-bind (type i1 i2)
      wallid 
    (let* ((a (ecase type
                (:vwall (maze-sn-walls maze))
                (:hwall (maze-we-walls maze))))
           (v (aref a i1 i2)))
      (setf (aref a i1 i2)
            (not v)))))

(defmacro with-event-button-handling ((object-reciever cursor-x cursor-y)
                                       &body code)
  "object-reciever must be a symbol which addresed to gtk object"
  (let ((event-sym (gensym "EVENT-"))
        (event-class-sym (gensym "EVENT-CLASS")))
    `(gir:connect ,object-reciever :button-press-event
                  (let ((,event-class-sym (gir:nget *gdk* "EventButton")))
                    (lambda (,object-reciever ,event-sym) 
                      (let* ((,event-sym (gir::build-struct-ptr ,event-class-sym
                                                                ,event-sym)))
                        (let ((,cursor-x (gir:field ,event-sym "x"))
                              (,cursor-y (gir:field ,event-sym "y")))
                          ,@code)))))))

(defun edit-maze-in-window! (maze)
  ;; COMMON-LISP:DIVISION-BY-ZERO with using of some usb mouses fix
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (let ((*maze-geometry* nil))
    (with-gtk-event-loop=window+drawing-area
        (window drawing-area
         :window-title "Maze editor")
      (gir:connect drawing-area :draw
                   (with-area-drawer-form (drawing-area context)
                     (draw-maze maze context)))
      (with-event-button-handling (drawing-area x y)
        (when *maze-geometry*
          (let ((wallid (clickpoint->wallid x y
                                            *maze-geometry*)))
            (when wallid
              (edit-wall wallid maze)
              (gir:invoke (drawing-area
                           :queue-draw)))))) 
      (register-events drawing-area
                       '(:button-press-mask))))
  maze)
