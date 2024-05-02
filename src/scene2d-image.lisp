(in-package #:eon-examples)

(eon:define-scene2d-constructed scene2d-image-example-screen-cell
    (scene2d-screen-cell
     :alignment (:start :end)
     :child (eon:scene2d-label :name image-information-label :string "")))

(defstruct scene2d-image-example-screen
  (cell (make-scene2d-image-example-screen-cell) :type eon:scene2d-constructed)
  (image (eon:scene2d-construct (eon:scene2d-image :drawable (eon:load-asset 'raylib:texture (example-asset #P"lisp-logo.png")))) :type eon:scene2d-image))

(defmethod eon:screen-render ((screen scene2d-image-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (let* ((image (scene2d-image-example-screen-image screen))
         (cell (scene2d-image-example-screen-cell screen))
         (label (scene2d-image-example-screen-cell-image-information-label cell)))
    (eon:scene2d-draw-simple image)
    (setf (eon:scene2d-label-string label) (format nil "~@{~A = ~A~^~%~}"
                                                   'position (eon:scene2d-position image)
                                                   'scale (eon:scene2d-scale image)
                                                   'origin (eon:scene2d-origin image)
                                                   'rotation (eon:scene2d-rotation image)))
    (cobj:with-monotonic-buffer-allocator (:size 128 :upstream nil)
      (eon:scene2d-layout cell))
    (eon:scene2d-draw-simple cell)))

(defun scene2d-image-example ()
  (let* ((screen (make-scene2d-image-example-screen))
         (image (scene2d-image-example-screen-image screen))
         (tween (let ((position (eon:scene2d-position image))
                      (scale (eon:scene2d-scale image))
                      (origin (eon:scene2d-origin image))
                      (size (eon:scene2d-size image)))
                  (symbol-macrolet ((rotation (eon:scene2d-rotation image)))
                    (ute:timeline
                     (:sequence
                      (:to (((raylib:vector2-x origin) (raylib:vector2-y origin))
                            ((/ (raylib:vector2-x size) 2.0) (/ (raylib:vector2-y size) 2.0)))
                       :duration 1.0)
                      (:to (((raylib:vector2-x position) (raylib:vector2-y position))
                            ((/ (coerce +viewport-width+ 'single-float) 2.0) (/ (coerce +viewport-height+ 'single-float) 2.0)))
                       :duration 1.0)
                      (:to (((raylib:vector2-x scale) (raylib:vector2-y scale)) (1.5 1.5))
                       :duration 1.0)
                      (:to ((rotation) (360.0))
                       :duration 1.0)
                      :repeat (:count t :yoyop t)))))))
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :initially (ute:start tween)
             :until (eq (await (eon:promise-pressed-key)) :b)
             :finally (ute:kill tween))))))

(pushnew 'scene2d-image-example *examples*)
