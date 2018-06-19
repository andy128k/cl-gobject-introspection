

(in-package :maze-example)

(defstruct maze
  height
  width
  sn-walls
  we-walls)

(defun make-empty-maze (width height)
  (let ((sn-walls (make-array (list (1+ width)
                                    height)
                              :initial-element nil))
        (we-walls (make-array (list (1+ height)
                                    width)
                              :initial-element nil))) 
    (make-maze :height height
               :width width
               :sn-walls sn-walls
               :we-walls we-walls)))

(defun make-basic-maze (width height)
  (let ((maze (make-empty-maze width height)))
    (with-slots (sn-walls we-walls)
        maze
      (macrolet (($border-building-loop (walls 
                                         border-width
                                         walls-count-1)
                   `(dotimes (i ,border-width)
                      (setf (aref ,walls 0 i)
                            t
                            (aref ,walls ,walls-count-1 i)
                            t))))
        ($border-building-loop sn-walls height width)
        ($border-building-loop we-walls width height)))
    maze))
