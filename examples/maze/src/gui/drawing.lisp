
(in-package :maze-example)

(defvar *gtk* (gir:require-namespace "Gtk"))
(defvar *gdk* (gir:require-namespace "Gdk"))

(defstruct (maze-geometry
            (:constructor make-maze-geometry
                (x y width height a nh nw)))
  nh nw height width x y a)

(defvar *maze-geometry*)

(defmacro with-gtk-event-loop=window+drawing-area
    ((window-symbol drawing-area-symbol
      &key
        protected-final-code
        (window-title "Untitled"))
     &body code)
  (let ((code `((gir:invoke (*gtk* 'init) nil)
                (let ((,window-symbol (gir:invoke (*gtk* "Window" 'new)
                                                  (gir:nget *gtk*
                                                            "WindowType"
                                                            :toplevel))) 
                      (,drawing-area-symbol (gir:invoke (*gtk* "DrawingArea"
                                                               'new))))
                  (setf (gir:property ,window-symbol 'title)
                        ,window-title)
                  (gir:invoke (,window-symbol "set_default_size")
                              900 900)
                  (gir:connect ,window-symbol :destroy
                               (lambda (win)
                                 (declare (ignore win)) 
                                 (gir:invoke (*gtk* 'main-quit)))) 
                  ,@code 
                  (gir:invoke (,window-symbol 'add)
                              ,drawing-area-symbol) 
                  (gir:invoke (,window-symbol 'show-all))
                  (gir:invoke (*gtk* 'main))
                  (null ,window-symbol)))))
    (if protected-final-code
        `(unwind-protect (progn ,@code)
           ,@protected-final-code)
        `(progn ,@code))))

(defmacro with-area-drawer-form ((drawing-area context-bind
                                  &optional (drawing-area-bind
                                             (gensym "DRAWING-AREA-")
                                             drawing-area-bind-p))
                                 &body code)
  (let ((context-ptr-sym (gensym "CONTEXT-POINTER-"))
        (size-requesters (list (gensym "WIDTH-REQUESTER-")
                               (gensym "HEIGHT-REQUESTER-"))))
    (flet (($size-setting-form (&rest hwlist)
             (mapcan (lambda (sym requester)
                       (list sym `(funcall ,requester)))
                     hwlist
                     size-requesters)))
      `(let (,context-bind
             ,@(mapcar (lambda (gir-method size-requester)
                         `(,size-requester (gir:nget ,drawing-area
                                                     ,gir-method)))
                       '("get_allocated_width"
                         "get_allocated_height")
                       size-requesters))
         (lambda (,drawing-area-bind ,context-ptr-sym)
           ,@(unless drawing-area-bind-p
               `((declare (ignore ,drawing-area-bind))))
           (if ,context-bind
               (with-slots ((h cairo:height)
                            (w cairo:width))
                   ,context-bind
                 (setf ,@($size-setting-form 'w 'h)))
               (setf ,context-bind
                     (make-instance 'cairo:context
                                    :pointer ,context-ptr-sym
                                    ,@($size-setting-form
                                       :width
                                       :height))))
           ,@code)))))

(defun draw-hline (y x1 x2)
  (cairo:move-to x1 y)
  (cairo:line-to x2 y)
  (cairo:stroke))

(defun draw-vline (x y1 y2)
  (cairo:move-to x y1)
  (cairo:line-to x y2)
  (cairo:stroke))


(defun draw-maze-grid (x0 y0 cell-side maze-width maze-height nw nh)
  (loop :for i :from 0 :to nh
        :for y = (+ y0 (* cell-side i))
        :with x1 = (+ x0 maze-width)
        :do (draw-hline y x0 x1))
  (loop :for i :from 0 :to nw 
        :for x = (+ x0 (* cell-side i))
        :with y1 = (+ y0 maze-height)
        :do (draw-vline x y0 y1)))


(defun draw-maze-walls (maze x0 y0 cell-side nw nh)
  (flet (($snwall (v n)
           (draw-vline (+ x0 (* v cell-side))
                       (+ y0 (* n cell-side))
                       (+ y0 (* (1+ n)
                                cell-side))))
         ($wewall (h n)
           (draw-hline (+ y0 (* h cell-side))
                       (+ x0 (* n cell-side))
                       (+ x0 (* (1+ n)
                                cell-side))))) 
    (with-slots (we-walls sn-walls)
        maze
      (loop :for h :from 0 :to nh
            :do (dotimes (n nw)
                  (when (aref we-walls h n)
                    ($wewall h n))))
      (loop :for v :from 0 :to nw
            :do (dotimes (n nh)
                  (when (aref sn-walls v n)
                    ($snwall v n)))))))

(defun draw-maze (maze context)
  (cairo:with-context (context)
    (with-slots ((nh height) (nw width))
        maze
      (let* ((ymax  (cairo:height context))
             (xmax (cairo:width context))
             (size (* 0.95 (min ymax xmax)))
             (cell-side (/ size (max nh nw)))) 
        (let* ((mazeh (* nh cell-side))
               (mazew (* nw cell-side)) 
               (y0 (/ (- ymax mazeh)
                      2))
               (x0 (/ (- xmax mazew)
                      2)))
          (cairo:set-source-rgb 1 1 1)
          (cairo:paint)
          (cairo:set-source-rgb 0.6 0.6 0.6)
          (cairo:set-line-width 5.0)
          (draw-maze-grid x0 y0 cell-side mazew mazeh nw nh)

          (cairo:set-source-rgb 0.1 0.1 0.1) 
          (cairo:set-line-width 10.0)
          (draw-maze-walls maze x0 y0 cell-side nw nh) 
          (setf *maze-geometry*
                (make-maze-geometry x0 y0 mazew mazeh cell-side nh nw)))))))

(defun register-events (obj
                        event-masks)
  (gir:invoke
   (obj 'set-events)
   (let ((e (gir:nget *gdk* "EventMask")))
     (reduce (lambda (s m)
               (boole boole-ior
                      s
                      (funcall e m)))
             (rest event-masks)
             :initial-value (funcall e (first event-masks))))))
