
(in-package :maze-example)

(defun event->key (ev)
  (let ((k (gir:field ev "keyval")) 
        (state (gir:field ev "state"))
        (modifiers (list)))
    (dolist (mdef '((#b1    :shift)
                    (#b100  :ctrl)
                    (#b1000 :alt)))
      (destructuring-bind (int modifier)
          mdef
        (unless (zerop (logand int state))
          (push modifier modifiers))))
    (values (case k
              (65361 :left-arrow)
              (65362 :up-arrow)
              (65363 :right-arrow)
              (65364 :down-arrow) 
              (otherwise k))
            modifiers)))


(let ((2pi (* 2 pi)))
  (defun draw-player-location (player-location cairo-context)
    (cairo:with-context (cairo-context)
      (destructuring-bind (x . y) player-location
        (with-slots (a (x0 x) (y0 y))
            *maze-geometry*
          (let ((x (+ x0 (* a (+ x 0.5))))
                (y (+ y0 (* a (+ y 0.5))))
                (r (* 0.4 a)))
            (cairo:set-source-rgb 0.984 0.129 0.129)
            (cairo:set-line-width 0.0)
            (cairo:arc x y r 0 2pi)
            (cairo:fill-path)))))))

(defun player-move (key player-location maze)
  (macrolet (($with-model (maze-binds &body code)
               `(with-slots ,maze-binds maze
                  (destructuring-bind (x . y) player-location
                    ,@code))))
    (case key
      (:up-arrow
       ($with-model (we-walls)
                    (unless (or (zerop y) (aref we-walls y x))
                      (cons x (- y 1)))))
      (:down-arrow
       ($with-model (we-walls height)
                    (unless (or (>= y (1- height))
                                (aref we-walls (1+ y) x))
                      (cons x (1+ y)))))
      (:left-arrow
       ($with-model (sn-walls)
                    (unless (or (zerop x) (aref sn-walls x y))
                      (cons (- x 1) y))))
      (:right-arrow
       ($with-model (sn-walls width)
                    (unless (or (>= x (1- width))
                                (aref sn-walls (1+ x) y))
                      (cons (1+ x) y))))
      (otherwise nil))))


(defmacro with-event-key-handling ((object-reciever key &optional modifiers)
                                   &body code)
  "object-reciever must be a symbol which addresed to gtk object"
  (let ((event-sym (gensym "EVENT-"))
        (event-class-sym (gensym "EVENT-CLASS")))
    `(gir:connect ,object-reciever :key-press-event
                  (let ((,event-class-sym (gir:nget *gdk* "EventKey")))
                    (lambda (,object-reciever ,event-sym) 
                      (let* ((,event-sym (gir::build-struct-ptr ,event-class-sym
                                                                ,event-sym)))
                        (multiple-value-bind (,key ,@(when modifiers
                                                       `(,modifiers)))
                            (event->key ,event-sym)
                          ,@code)))))))



(defun walk-through-maze (maze &optional (x0 0) (y0 0)) 
  (let ((*maze-geometry* nil)
        (player-location (cons x0 y0)))
    (with-gtk-event-loop=window+drawing-area
        (window drawing-area
         :window-title "Maze")
      (let ((id nil)
            (hided-p nil)
            (drawer (with-area-drawer-form (drawing-area context)
                      (draw-maze maze context)
                      (draw-player-location player-location
                                            context)))
            (drawerh (with-area-drawer-form (drawing-area context)
                       (draw-maze maze context))))
        (flet (($switch-mode (drawer)
                 (when id
                   (gir:disconnect drawing-area id))
                 (setf id
                       (gir:connect drawing-area :draw
                                    drawer))))
          ($switch-mode drawer)
          (with-event-key-handling (window key mod)
            (format *standard-output*
                    "Key: ~A~%"
                    (list key mod))
            (unless hided-p
              (let ((location (player-move key player-location maze)))
                (when location
                  (setf player-location location)
                  (gir:invoke (drawing-area
                               :queue-draw)))))
            (cond
              ((and (eq 113 key) 
                    (member :ctrl mod)) ; 'C-q'
               (gir:invoke (window :close)))
              ((eq 72 key) ; 'H' key
               (if hided-p
                   ($switch-mode drawer)
                   ($switch-mode drawerh))
               (setf hided-p (not hided-p))
               (gir:invoke (drawing-area
                            :queue-draw))))))) 
      (register-events window
                       '(:key-press-mask)))))
