(in-package #:eon-examples)

(eon:define-scene2d-constructed parallax-background-example-screen-cell
    (scene2d-screen-cell
     :child (eon:scene2d-group
             :children ((eon:scene2d-tile-scroll-region
                         :name background-1
                         :style (eon:scene2d-tile-scroll-region-style
                                 :tile-scroll-style (eon:scene2d-tile-scroll-style :tile (raylib:texture (example-asset #P"parallax-1.png"))))
                         :size (#.+viewport-width+ #.+viewport-height+))
                        (eon:scene2d-tile-scroll-region
                         :name background-2
                         :style (eon:scene2d-tile-scroll-region-style
                                 :tile-scroll-style (eon:scene2d-tile-scroll-style :tile (raylib:texture (example-asset #P"parallax-2.png"))))
                         :size (#.+viewport-width+ #.+viewport-height+))))))

(defstruct parallax-background-example-screen
  (cell (make-parallax-background-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen parallax-background-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (let ((cell (parallax-background-example-screen-cell screen)))
    (let ((offset (eon:scene2d-tile-scroll-region-offset (parallax-background-example-screen-cell-background-1 cell))))
      (incf (raylib:vector2-x offset) (* 32.0 (eon:game-loop-delta-time))))
    (let ((offset (eon:scene2d-tile-scroll-region-offset (parallax-background-example-screen-cell-background-2 cell))))
      (incf (raylib:vector2-x offset) (* 64.0 (eon:game-loop-delta-time))))
    (eon:scene2d-draw-simple cell)))

(defun parallax-background-example ()
  (let* ((screen (make-parallax-background-example-screen))
         (cell (parallax-background-example-screen-cell screen)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async (loop :until (eq (await (eon:promise-pressed-key)) :b))))))

(pushnew 'parallax-background-example *examples*)
