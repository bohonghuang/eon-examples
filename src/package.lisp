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
        (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+))))))
