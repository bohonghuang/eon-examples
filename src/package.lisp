(defpackage eon-examples
  (:use #:cl #:cffi-ops #:alexandria #:promise-async-await)
  (:export #:main))

(in-package #:eon-examples)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +viewport-width+ 320)
  (defconstant +viewport-height+ 192))

(defstruct (example-menu-background-cell (:include eon:scene2d-layout)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (eon:define-scene2d-default-construct-form example-menu-background-cell (size)))

(defmethod eon:scene2d-draw ((cell example-menu-background-cell) position origin scale rotation tint)
  (declare (ignore origin scale rotation))
  (let ((size (example-menu-background-cell-size cell)))
    (raylib:draw-rectangle-lines
     (floor (raylib:vector2-x position))
     (floor (raylib:vector2-y position))
     (floor (raylib:vector2-x size))
     (floor (raylib:vector2-y size))
     tint)))

(defun make-scene2d-screen-cell (&key child (content child) (alignment (eon::make-scene2d-alignment)))
  (eon:scene2d-construct
   (eon:scene2d-cell :alignment alignment :size (320 192) :child (eon:scene2d-coordinate-truncator :child content))))

(defun scene2d-screen-cell-child (cell)
  (eon::scene2d-coordinate-truncator-content (eon::scene2d-cell-content cell)))

(eon:define-scene2d-default-construct-form scene2d-screen-cell (child alignment))

(defstruct (scene2d-rectangle (:include eon:scene2d-layout)))

(defmethod eon:scene2d-draw ((rectangle scene2d-rectangle) position origin scale rotation tint)
  (let ((size (scene2d-rectangle-size rectangle)))
    (clet ((rectangle (foreign-alloca '(:struct raylib:rectangle))))
      (setf (-> rectangle raylib:x) (raylib:vector2-x position)
            (-> rectangle raylib:y) (raylib:vector2-y position)
            (-> rectangle raylib:width) (* (raylib:vector2-x size) (raylib:vector2-x scale))
            (-> rectangle raylib:height) (* (raylib:vector2-y size) (raylib:vector2-y scale)))
      (raylib:%draw-rectangle-pro rectangle (& origin) rotation (& tint))))
  (call-next-method))

(eon:define-scene2d-default-construct-form scene2d-rectangle (size))

(defun example-menu-background (&optional (size 16.0))
  (eon:scene2d-construct
   (eon:scene2d-tile-scroll-region
    :style (eon:scene2d-tile-scroll-region-style :tile-scroll-style (eon:scene2d-tile-scroll-style :tile (example-menu-background-cell :size (raylib:vector2-scale (raylib:vector2-one) size) :color (raylib:fade raylib:+gray+ 0.5))))
    :size (#.(float +viewport-width+) #.(float +viewport-height+)))))

(defvar *examples* nil)

(defun example-asset (pathname)
  (merge-pathnames pathname #.(merge-pathnames #P"assets/" (asdf:component-pathname (asdf:find-system '#:eon-examples)))))

(defun promise-transition-example-screen (screen)
  (eon:promise-transition-screen screen 'example-screen-transition-out 'example-screen-transition-in))

(defmacro save-screen-excursion (&body body)
  (with-gensyms (screen)
    `(let ((,screen (eon:current-screen)))
       (prog1 (progn . ,body)
         (unless (eq (eon:current-screen) ,screen)
           (await (promise-transition-example-screen ,screen)))))))

(defstruct hq2x
  (shader (eon:load-asset 'raylib:shader (example-asset #P"hq2x")) :type raylib:shader)
  (shader-uniforms (make-hq2x-shader-uniforms) :type cobj:cobject))

(eon:define-shaderable-uniforms hq2x
  ("inputSize" (raylib:make-vector2
                :x (float +viewport-width+)
                :y (float +viewport-height+))
               :type raylib:vector2))

(defun hq2x-processor ()
  (let ((hq2x (make-hq2x)))
    (lambda (draw-function)
      (raylib:with-shader-mode (hq2x-shader hq2x)
        (update-hq2x-shader-uniforms hq2x)
        (funcall draw-function)))))

(defstruct lcd3x
  (shader (eon:load-asset 'raylib:shader (example-asset #P"lcd3x")) :type raylib:shader)
  (shader-uniforms (make-lcd3x-shader-uniforms) :type cobj:cobject))

(eon:define-shaderable-uniforms lcd3x
  ("inputSize" (raylib:make-vector2
                :x (float +viewport-width+)
                :y (float +viewport-height+))
               :type raylib:vector2))

(defun lcd3x-processor ()
  (let ((lcd3x (make-lcd3x)))
    (lambda (draw-function)
      (raylib:with-shader-mode (lcd3x-shader lcd3x)
        (update-lcd3x-shader-uniforms lcd3x)
        (funcall draw-function)))))

(defstruct hq4x
  (shader (eon:load-asset 'raylib:shader (example-asset #P"hq4x")) :type raylib:shader)
  (shader-uniforms (make-hq4x-shader-uniforms) :type cobj:cobject))

(eon:define-shaderable-uniforms hq4x
  ("inputSize" (raylib:make-vector2
                :x (float +viewport-width+)
                :y (float +viewport-height+))
               :type raylib:vector2))

(defun hq4x-processor ()
  (let ((hq4x (make-hq4x)))
    (lambda (draw-function)
      (raylib:with-shader-mode (hq4x-shader hq4x)
        (update-hq4x-shader-uniforms hq4x)
        (funcall draw-function)))))

(defun main ()
  (catch 'exit
    (raylib:set-config-flags (cffi:foreign-bitfield-value 'raylib:config-flags '(:window-resizable)))
    (raylib:with-window ("EON - Examples" ((* +viewport-width+ 2) (* +viewport-height+ 2)))
      (raylib:set-target-fps 60)
      (eon:with-game-context
        (let* ((group (eon:scene2d-construct (eon:scene2d-group)))
               (screen (let ((background (example-menu-background)))
                         (eon:scene2d-layout background)
                         (lambda ()
                           (raylib:clear-background raylib:+white+)
                           (let ((offset (eon:scene2d-tile-scroll-region-offset background)))
                             (setf (raylib:vector2-y offset) (incf (raylib:vector2-x offset) (* 16.0 (eon:game-loop-delta-time)))))
                           (eon:scene2d-draw-simple background)
                           (eon:scene2d-draw-simple group)))))
          (let* ((eon.debug:*debug-window-group* group)
                 (*async-continuation-constructor* (async-special-variable-binder (eon.debug:*debug-window-group*))))
            (async
              (if (fboundp 'logo-example)
                  (progn
                    (await (funcall 'logo-example))
                    (await (eon:promise-transition-screen
                            screen
                            (eon:make-screen-transition :out 'eon:screen-transition-fade :duration 0.0 :background raylib:+white+)
                            (eon:make-screen-transition :in 'eon:screen-transition-fade :duration 0.25 :background raylib:+white+))))
                  (setf (eon:current-screen) screen))
              (loop :for example := (await (eon.debug:promise-selection "Select an example." (reverse *examples*) example))
                    :while example
                    :do (save-screen-excursion (await (funcall example))))
              (throw 'exit nil))))
        (eon:do-screen-loop
            (eon:make-post-effect-viewport
             :width +viewport-width+
             :height +viewport-height+
             :processor (let ((size 2)
                              (1x #'funcall)
                              (hq2x (hq2x-processor))
                              (lcd3x (lcd3x-processor))
                              (hq4x (hq4x-processor)))
                          (declare (type (mod 8) size))
                          (eon:add-game-loop-hook
                           (lambda ()
                             (cond
                               ((raylib:is-key-pressed #.(cffi:foreign-enum-value 'raylib:keyboard-key :equal))
                                (setf size (min (1+ size) 4))
                                (raylib:set-window-size (* +viewport-width+ size) (* +viewport-height+ size)))
                               ((raylib:is-key-pressed #.(cffi:foreign-enum-value 'raylib:keyboard-key :minus))
                                (setf size (max (1- size) 1))
                                (raylib:set-window-size (* +viewport-width+ size) (* +viewport-height+ size)))))
                           :after t)
                          (lambda (draw-function)
                            (rlgl:load-identity)
                            (rlgl:scalef 4.0 4.0 1.0)
                            (funcall
                             (ecase size
                               (1 1x)
                               (2 hq2x)
                               (3 lcd3x)
                               (4 hq4x))
                             draw-function)))
             :viewport (eon:make-fit-viewport
                        :width (* +viewport-width+ 4)
                        :height (* +viewport-height+ 4))))))))
