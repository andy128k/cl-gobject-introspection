(in-package :flood-game-example)

(defvar *glib* (gir:require-namespace "GLib"))
(defvar *gtk* (gir:require-namespace "Gtk" "3.0"))

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
           (cairo:with-context (,context-bind)
             ,@code))))))

(cffi:defcallback timeout-func :boolean ((data :pointer))
  (funcall (gir:trampoline-get-function data)))

(defun gtk-window-with-cairo-painting (drawer) 
  (let ((timeout-id nil))
    (with-gtk-event-loop=window+drawing-area
        (window drawing-area
                :window-title "Flood game example"
                :protected-final-code
                ((when timeout-id
                   (gir:invoke (*glib* 'source-remove) timeout-id))))
      (gir:connect window :realize
                   (lambda (window) 
                     (flet (($redraw ()
                              (gir:invoke (window :queue-draw))
                              t))
                       (setf timeout-id
			     (gir:invoke (*glib* 'timeout-add)
					 0
					 300
					 (cffi:callback timeout-func)
					 (gir:make-trampoline #'$redraw)
					 (cffi:callback gir:destroy-trampoline))))))
      (gir:connect drawing-area :draw
                   (with-area-drawer-form (drawing-area context)
                     drawing-area
                     (funcall drawer context))))))
