(in-package #:eon-examples)

(eon:define-scene2d-constructed scene2d-window-example-screen-cell
    (scene2d-screen-cell
     :child (eon:scene2d-box
             :orientation :vertical
             :children ((eon:scene2d-window
                         :child (eon:scene2d-margin
                                 :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                                 :child (eon:scene2d-label :string "Default Window Style")))
                        (eon:scene2d-window
                         :child (eon:scene2d-margin
                                 :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                                 :child (eon:scene2d-label :string "Custom Window Style"))
                         :style (eon:scene2d-window-style
                                 :background (eon:n-patch :texture (raylib:texture (example-asset #P"window-border-1.png"))
                                                          :body (3.0 3.0 -3.0 -3.0))))))))

(defstruct scene2d-window-example-screen
  (cell (make-scene2d-window-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen scene2d-window-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (scene2d-window-example-screen-cell screen)))

(defun scene2d-window-example ()
  (let* ((screen (make-scene2d-window-example-screen))
         (cell (scene2d-window-example-screen-cell screen)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async (loop :until (eq (await (eon:promise-pressed-controller-button)) :b))))))

(pushnew 'scene2d-window-example *examples*)
