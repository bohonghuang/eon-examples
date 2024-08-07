(in-package #:eon-examples)

(eon:define-scene2d-constructed scene2d-label-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-box
           :orientation :vertical
           :children ((eon:scene2d-label :string "Default Label Style")
                      (eon:scene2d-label
                       :string "Label Style Without Shadow"
                       :style (eon:scene2d-label-style
                               :shadow nil))
                      (eon:scene2d-label
                       :string "Label Style With Outline"
                       :style (eon:scene2d-label-style
                               :shadow nil
                               :outline raylib:+gray+))
                      (eon:scene2d-label
                       :string "Label Style With Red Color"
                       :style (eon:scene2d-label-style :color raylib:+red+))
                      (eon:scene2d-label
                       :string "Label Style With Custom Font"
                       :style (eon:scene2d-label-style
                               :text-style (eon:text-style
                                            :font (raylib:font (example-asset #P"clacon2.ttf"))
                                            :size 16.0
                                            :spacing 1.0)))))))

(defstruct scene2d-label-example-screen
  (cell (make-scene2d-label-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen scene2d-label-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (scene2d-label-example-screen-cell screen)))

(defun scene2d-label-example ()
  (let* ((screen (make-scene2d-label-example-screen))
         (cell (scene2d-label-example-screen-cell screen)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async (loop :until (eq (await (eon:promise-pressed-controller-button)) :b))))))

(pushnew 'scene2d-label-example *examples*)
