(in-package #:eon-examples)

(eon:define-scene2d-constructed virtual-keyboard-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-window
           :child (eon:scene2d-margin
                   :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                   :child (eon:virtual-keyboard :name virtual-keyboard)))))

(defstruct virtual-keyboard-example-screen
  (cell (make-virtual-keyboard-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen virtual-keyboard-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (virtual-keyboard-example-screen-cell screen)))

(defun virtual-keyboard-example ()
  (let* ((screen (make-virtual-keyboard-example-screen))
         (cell (virtual-keyboard-example-screen-cell screen))
         (keyboard (virtual-keyboard-example-screen-cell-virtual-keyboard cell)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :for char := (await (eon:virtual-keyboard-promise-char keyboard))
             :while char
             :do (print char))))))

(pushnew 'virtual-keyboard-example *examples*)
