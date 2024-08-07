(in-package #:eon-examples)

(eon:define-scene2d-constructed scene2d-layout-example-entry ()
  (eon:scene2d-coordinate-truncator
   :child (eon:scene2d-window
           :child (eon:scene2d-box
                   :name box
                   :orientation :vertical
                   :children ((eon:scene2d-label :name title-label :string "")))))
  (:constructor (&key title body)
      (let ((entry (%make-scene2d-layout-example-entry)))
        (setf (eon:scene2d-label-string (scene2d-layout-example-entry-title-label entry)) title)
        (eon:scene2d-box-add-child (scene2d-layout-example-entry-box entry) body)
        entry)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (eon:define-scene2d-default-construct-form scene2d-layout-example-entry (title body)))

(defstruct (scene2d-layout-example-rectangle (:include scene2d-rectangle)))

(defmethod eon:scene2d-layout ((rectangle scene2d-layout-example-rectangle))
  (let ((size (scene2d-layout-example-rectangle-size rectangle)))
    (when (zerop (raylib:vector2-length size))
      (setf (raylib:vector2-x size) (+ 16.0 (random 8))
            (raylib:vector2-y size) (+ 8.0 (random 4))))
    (raylib:copy-color
     (raylib:get-color (logior (ash (random #xFFFFFF) 8) #x000000FF))
     (scene2d-layout-example-rectangle-color rectangle)))
  (call-next-method))

(eon:define-scene2d-constructed scene2d-layout-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-flow-box
           :size (#.+viewport-width+ #.+viewport-height+)
           :orientation :vertical
           :alignment (:center :center)
           :children ((scene2d-layout-example-entry
                       :title (format nil "~A (~A)" 'eon:scene2d-box :vertical)
                       :body (eon:scene2d-box
                              :orientation :vertical
                              :children (loop :repeat 3 :collect (make-scene2d-layout-example-rectangle))))
                      (scene2d-layout-example-entry
                       :title (format nil "~A (~A)" 'eon:scene2d-box :horizontal)
                       :body (eon:scene2d-box
                              :orientation :horizontal
                              :children (loop :repeat 3 :collect (make-scene2d-layout-example-rectangle))))
                      (scene2d-layout-example-entry
                       :title (format nil "~A" 'eon:scene2d-flow-box)
                       :body (eon:scene2d-flow-box
                              :size (160.0 0.0)
                              :orientation :vertical
                              :alignment (:start :center)
                              :children (loop :repeat 15 :collect (make-scene2d-layout-example-rectangle))))
                      (scene2d-layout-example-entry
                       :title (format nil "~A" 'eon:scene2d-table)
                       :body (loop :with table := (eon:scene2d-construct (eon:scene2d-table :orientation :vertical))
                                   :for i :below 12
                                   :when (zerop (mod i 4))
                                     :do (eon:scene2d-table-newline table)
                                   :do (eon:scene2d-table-add-child table (make-scene2d-layout-example-rectangle))
                                   :finally (return table)))
                      (scene2d-layout-example-entry
                       :title (format nil "~A" 'eon:scene2d-cell)
                       :body (eon:scene2d-cell
                              :alignment (:end :center)
                              :child (make-scene2d-layout-example-rectangle)
                              :size (80 40)))
                      (scene2d-layout-example-entry
                       :title (format nil "~A" 'eon:scene2d-group)
                       :body (eon:scene2d-group
                              :children ((make-scene2d-layout-example-rectangle :position (raylib:make-vector2 :x 0.0 :y 0.0))
                                         (make-scene2d-layout-example-rectangle :position (raylib:make-vector2 :x 4.0 :y 4.0))
                                         (make-scene2d-layout-example-rectangle :position (raylib:make-vector2 :x 8.0 :y 8.0)))))
                      (scene2d-layout-example-entry
                       :title (format nil "~A" 'eon:scene2d-margin)
                       :body (eon:scene2d-margin
                              :top 8.0 :bottom 8.0 :left 8.0 :right 8.0
                              :child (make-scene2d-layout-example-rectangle)))))))

(defstruct scene2d-layout-example-screen
  (cell (make-scene2d-layout-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen scene2d-layout-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (scene2d-layout-example-screen-cell screen)))

(defun scene2d-layout-example ()
  (let* ((screen (make-scene2d-layout-example-screen))
         (cell (scene2d-layout-example-screen-cell screen)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async (loop :until (eq (await (eon:promise-pressed-controller-button)) :b))))))

(pushnew 'scene2d-layout-example *examples*)
