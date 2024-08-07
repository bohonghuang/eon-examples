(in-package #:eon-examples)

(eon:define-scene2d-constructed input-field-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-window
           :child (eon:scene2d-margin
                   :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                   :child (eon:scene2d-max-cell
                           :size (160.0 0.0)
                           :alignment (:start :center)
                           :child (eon:input-field :name input-field))))))

(defstruct input-field-example-screen
  (cell (make-input-field-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen input-field-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (input-field-example-screen-cell screen)))

(defun input-field-example ()
  (let* ((screen (make-input-field-example-screen))
         (cell (input-field-example-screen-cell screen))
         (input-field (input-field-example-screen-cell-input-field cell)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :for line := (await (eon:input-field-promise-line input-field))
             :until (emptyp line)
             :do (format t "You entered ~S.~%" line))))))

(pushnew 'input-field-example *examples*)
