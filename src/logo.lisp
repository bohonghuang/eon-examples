(in-package #:eon-examples)

(eon:define-scene2d-constructed logo-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-shaderable-container
           :name shaderable-container
           :shader (raylib:shader (example-asset #P"text-shine"))
           :shader-uniforms (make-logo-example-screen-cell-shader-uniforms)
           :child (eon:scene2d-box
                   :name root-box
                   :orientation :vertical
                   :children ((eon:scene2d-box
                               :name logo-box
                               :orientation :horizontal
                               :children (loop :for char :across "EONExamples"
                                               :for string := (string char)
                                               :collect (eon:scene2d-construct (eon:scene2d-margin :right (if (char= char #\N) 4.0 0.0) :right 0.0 :child (eon:scene2d-label :string string :style (eon:scene2d-label-style :text-style (eon:text-style :size 50.0 :font (raylib:font (example-asset #P"clacon2.ttf"))) :shadow nil :color raylib:+white+))))))
                              (eon:scene2d-margin :top 24.0 :child (eon:scene2d-label :name author-label :string "bohonghuang" :style (eon:scene2d-label-style :text-style (eon:text-style :size 20.0) :shadow nil :color (255 0 255 255)))))))))

(defun logo-example-screen-cell-shader (cell)
  (eon::scene2d-shaderable-container-shader (logo-example-screen-cell-shaderable-container cell)))

(defun logo-example-screen-cell-shader-uniforms (cell)
  (eon::scene2d-shaderable-container-shader-uniforms (logo-example-screen-cell-shaderable-container cell)))

(eon:define-shaderable-uniforms logo-example-screen-cell
  ("size" (raylib:make-vector2 :x (float +viewport-width+) :y (float +viewport-height+)) :type raylib:vector2)
  ("shineMask" (let ((texture (eon:load-asset 'raylib:texture (example-asset #P"text-shine-mask-1.png"))))
                 (raylib:set-texture-wrap texture #.(cffi:foreign-enum-value 'raylib:texture-wrap :clamp))
                 texture)
               :type raylib:texture)
  ("shineOffset" (raylib:make-vector2 :x -0.5 :y 0.0) :type raylib:vector2))

(defstruct logo-example-screen
  (cell (make-logo-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen logo-example-screen))
  (raylib:clear-background raylib:+white+)
  (eon:scene2d-draw-simple (logo-example-screen-cell screen)))

(progn
  #1=(progn
       (defun color-h (color)
         (clet ((hsv (foreign-alloca '(:struct raylib:vector3))))
           (raylib:%color-to-hsv (& hsv) (& color))
           (values (-> hsv raylib:x))))
       (defun (setf color-h) (value color)
         (clet ((hsv (foreign-alloca '(:struct raylib:vector3))))
           (raylib:%color-to-hsv (& hsv) (& color))
           (setf (values (-> hsv raylib:x)) value)
           (let ((a (raylib:color-a color)))
             (raylib:%color-from-hsv (& color) (-> hsv raylib:x) (-> hsv raylib:y) (-> hsv raylib:z))
             (setf (raylib:color-a color) a))
           value)))
  #.(subst '(values (-> hsv raylib:y)) '(values (-> hsv raylib:x)) (subst 'color-s 'color-h '#1#) :test #'equal)
  #.(subst '(values (-> hsv raylib:z)) '(values (-> hsv raylib:x)) (subst 'color-v 'color-h '#1#) :test #'equal))

(defun logo-example-animation-popup (box)
  (loop :with timeline := (ute:timeline (:parallel))
        :with center-x :of-type single-float := (/ (raylib:vector2-x (eon:scene2d-size box)) 2.0)
        :for child :in (eon::scene2d-box-content box)
        :for index :from 0
        :do (let ((position (eon:scene2d-position child))
                  (color (eon:scene2d-color child))
                  (scale (eon:scene2d-scale child)))
              (let ((color-h+120 (+ (color-h color) 120.0)))
                (flet ((color-h+120 (color) (declare (ignore color)) color-h+120)
                       ((setf color-h+120) (value color) (setf (color-h color) (mod (- (setf color-h+120 value) 120.0) 360.0))))
                  (ute:add-child timeline (ute:timeline
                                           (:sequence
                                            (:to (((eon:integer-float (raylib:color-a color))) (0.0)))
                                            (:pause (* index 0.1))
                                            (:parallel
                                             (:parallel
                                              (:from (((raylib:vector2-x scale) (raylib:vector2-y scale))
                                                      (4.0 4.0))
                                               :duration 1.0
                                               :ease #'ute:quad-out))
                                             (:parallel
                                              (:from (((raylib:vector2-y position))
                                                      (-72.0)
                                                      (128.0))
                                               :relativep t
                                               :duration 1.0
                                               :ease #'ute:quad-out)
                                              (:to (((raylib:vector2-y position))
                                                    (-8.0)
                                                    (0.0))
                                               :relativep t
                                               :duration 1.0
                                               :ease #'ute:bounce-out
                                               :delay 0.8)
                                              (:from (((raylib:vector2-x position))
                                                      ((+ (* (- (raylib:vector2-x position) center-x) 0.3333) center-x))
                                                      ((+ (* (- (raylib:vector2-x position) center-x) 1.6666) center-x)))
                                               :duration 1.0
                                               :ease #'ute:linear-inout))
                                             (:sequence
                                              (:to (((color-h+120 color) (color-s color) (color-v color) (eon:integer-float (raylib:color-a color)))
                                                    (0.0 0.0 0.98 255.0)))
                                              (:parallel
                                               (:to (((color-s color))
                                                     (1.0))
                                                :duration 0.25
                                                :ease #'ute:quad-in)
                                               (:to (((color-h+120 color))
                                                     (360.0))
                                                :duration 1.0
                                                :ease #'ute:linear-inout))))))))))
        :finally (return timeline)))

(defun logo-example-animation-shine (uniforms)
  (let ((offset (logo-example-screen-cell-shader-uniforms-shine-offset uniforms)))
    (ute:timeline
     (:sequence
      (:to (((raylib:vector2-x offset)) (1.0))
       :duration 2.0)))))

(defun logo-example ()
  (let* ((screen (make-logo-example-screen))
         (cell (logo-example-screen-cell screen))
         (box (logo-example-screen-cell-logo-box cell))
         (label (logo-example-screen-cell-author-label cell))
         (uniforms (logo-example-screen-cell-shader-uniforms cell)))
    (eon:scene2d-layout cell)
    (nreversef (eon::scene2d-box-content (logo-example-screen-cell-root-box cell)))
    (let ((timeline
            (ute:timeline
             (:sequence
              (:to (((eon:integer-float (raylib:color-a (eon:scene2d-color label)))) (0.0)))
              (:pause 0.5)
              (:to (((eon:integer-float (raylib:color-a (eon:scene2d-color label)))) (255.0)))
              (:pause 0.25)
              (:tween (logo-example-animation-popup box))
              (:pause -0.5)
              (:tween (logo-example-animation-shine uniforms))
              (:pause -0.25)
              (:to (((eon:integer-float (raylib:color-a (eon:scene2d-color label)))
                     (eon:integer-float (raylib:color-a (eon:scene2d-color box))))
                    (0.0 0.0))
               :duration 0.5
               :ease #'ute:linear-inout)))))
      (ajoin
       (if (eon:current-screen)
           (promise-transition-example-screen screen)
           (async (setf (eon:current-screen) screen)))
       (eon:promise-tween timeline)))))

(pushnew 'logo-example *examples*)
