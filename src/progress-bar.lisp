(in-package #:eon-examples)

(eon:define-scene2d-constructed progress-bar-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-box
           :children ((eon:progress-bar
                       :name progress-bar-1
                       :value 0.0
                       :style (eon:progress-bar-style
                               :track (eon:scene2d-rectangle :position (0.0 0.0) :size (100.0 10.0) :color raylib:+gray+)
                               :track-region (raylib:make-rectangle :x 1.0 :y 1.0 :width 98.0 :height 8.0)
                               :thumb (eon:scene2d-image :drawable (eon:load-asset 'raylib:texture (example-asset #P"lisp-logo.png")) :scale (0.125 0.125))
                               :thumb-alignment :edge
                               :fill (eon:scene2d-rectangle :position (0.0 0.0) :size (100.0 10.0) :color raylib:+green+)))
                      (eon:scene2d-cell :size (0.0 16.0))
                      (eon:progress-bar
                       :name progress-bar-2
                       :value 0.0
                       :style (eon:progress-bar-style
                               :track (eon:scene2d-rectangle :position (0.0 0.0) :size (100.0 10.0) :color raylib:+gray+)
                               :track-region (raylib:make-rectangle :x 1.0 :y 1.0 :width 98.0 :height 8.0)
                               :thumb (eon:scene2d-image :drawable (eon:load-asset 'raylib:texture (example-asset #P"lisp-logo.png")) :scale (0.125 0.125))
                               :thumb-alignment :center
                               :fill (eon:scene2d-rectangle :position (0.0 0.0) :size (100.0 10.0) :color raylib:+red+)))))))

(defstruct progress-bar-example-screen
  (cell (make-progress-bar-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen progress-bar-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (progress-bar-example-screen-cell screen)))

(defun progress-bar-example ()
  (let* ((screen (make-progress-bar-example-screen))
         (cell (progress-bar-example-screen-cell screen))
         (progress-bar-1 (progress-bar-example-screen-cell-progress-bar-1 cell))
         (progress-bar-2 (progress-bar-example-screen-cell-progress-bar-2 cell))
         (tween (ute:tween
                 :to (((eon:progress-bar-value progress-bar-1)
                       (eon:progress-bar-value progress-bar-2))
                      (1.0 1.0))
                 :ease #'ute:sine-in :duration 1.0
                 :repeat (:count t :yoyop t))))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :initially (ute:start tween)
             :until (eq (await (eon:promise-pressed-controller-button)) :b)
             :finally (ute:kill tween))))))

(pushnew 'progress-bar-example *examples*)
