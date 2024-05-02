(in-package #:eon-examples)

(eon:define-simple-shader-screen-transition (screen-transition-mosaic (example-asset #P"mosaic.fs"))
  ("width" (float +viewport-width+) :type single-float)
  ("height" (float +viewport-height+) :type single-float)
  ("intensity" 16.0 :type single-float))

(eon:define-simple-shader-screen-transition (screen-transition-wipe-iris (example-asset #P"mask-wipe.fs"))
  ("mask" (eon:load-asset 'raylib:texture (example-asset #P"mask-wipe-iris.png")) :type raylib:texture)
  ("background" raylib:+black+ :type raylib:color)
  ("smoothness" 0.0 :type single-float))

(eon:define-simple-shader-screen-transition (screen-transition-wipe-clock (example-asset #P"mask-wipe.fs"))
  ("mask" (eon:load-asset 'raylib:texture (example-asset #P"mask-wipe-clock.png")) :type raylib:texture)
  ("background" raylib:+black+ :type raylib:color)
  ("smoothness" 0.0 :type single-float)
  ("flipXP" nil :type boolean)
  ("flipYP" nil :type boolean))

(eon:define-simple-shader-screen-transition (screen-transition-wipe-vertical (example-asset #P"mask-wipe.fs"))
  ("mask" (eon:load-asset 'raylib:texture (example-asset #P"mask-wipe-vertical.png")) :type raylib:texture)
  ("background" raylib:+black+ :type raylib:color)
  ("smoothness" 0.0 :type single-float)
  ("flipYP" nil :type boolean))

(eon:define-simple-shader-screen-transition (screen-transition-wipe-horizontal (example-asset #P"mask-wipe.fs"))
  ("mask" (eon:load-asset 'raylib:texture (example-asset #P"mask-wipe-horizontal.png")) :type raylib:texture)
  ("background" raylib:+black+ :type raylib:color)
  ("smoothness" 0.0 :type single-float)
  ("flipXP" nil :type boolean))

(defun screen-transition-example ()
  (let ((screen (make-tiled-map-example-screen))
        (smoothness 0.0))
    (let* ((eon:*tiled-renderer-camera* (tiled-map-example-screen-camera screen))
           (*async-continuation-constructor* (async-special-variable-binder (eon:*tiled-renderer-camera*))))
      (async
        (eon.debug:with-popped-prompt "Loading..."
          (setf (tiled-map-example-screen-renderer screen) (eon:tiled-map-renderer (await (eon:promise-task (curry #'tiled:load-map (example-asset #P"MagicLand.tmx")))))))
        (await (promise-transition-example-screen screen))
        (let* ((eon.debug:*debug-window-group* (tiled-map-example-screen-group screen))
               (*async-continuation-constructor* (async-special-variable-binder (eon.debug:*debug-window-group*))))
          (eon.debug:selection-case 'eon::screen-transition
            (eon:screen-transition-fade
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'eon:screen-transition-fade :duration 0.25)
               (eon:make-screen-transition :in 'eon:screen-transition-fade :duration 0.25))))
            (screen-transition-mosaic
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'screen-transition-mosaic :duration 0.5)
               (eon:make-screen-transition :in 'screen-transition-mosaic :duration 0.5))))
            (--toggle-smooth-wiping--
             (await (eon.debug:promise-hint (format nil "Smooth wiping is ~:[disabled~;enabled~]." (plusp (setf smoothness (if (plusp smoothness) 0.0 0.5)))))))
            (screen-transition-wipe-iris
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'screen-transition-wipe-iris :duration 0.5 :smoothness smoothness)
               (eon:make-screen-transition :in 'screen-transition-wipe-iris :duration 0.5 :smoothness smoothness))))
            (screen-transition-wipe-clock
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'screen-transition-wipe-clock :smoothness smoothness)
               (eon:make-screen-transition :in 'screen-transition-wipe-clock :smoothness smoothness))))
            (screen-transition-wipe-vertical
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'screen-transition-wipe-vertical :duration 0.5 :smoothness smoothness)
               (eon:make-screen-transition :in 'screen-transition-wipe-vertical :duration 0.5 :smoothness smoothness))))
            (screen-transition-wipe-horizontal
             (await
              (eon:promise-transition-screen
               (eon:current-screen)
               (eon:make-screen-transition :out 'screen-transition-wipe-horizontal :duration 0.5 :smoothness smoothness)
               (eon:make-screen-transition :in 'screen-transition-wipe-horizontal :duration 0.5 :smoothness smoothness))))))))))

(pushnew 'screen-transition-example *examples*)

(progn
  #1=(defmethod eon::ensure-screen-transition ((object (eql 'example-screen-transition-out)))
       (let ((transition-fade (eon:make-screen-transition nil 'eon:screen-transition-fade))
             (transition-mosaic (eon:make-screen-transition nil 'screen-transition-mosaic)))
         (values
          (let ((transition-fade (eon::ensure-screen-transition transition-fade))
                (transition-mosaic (eon::ensure-screen-transition transition-mosaic)))
            (lambda (screen-manager)
              (funcall transition-mosaic screen-manager)
              (funcall transition-fade screen-manager)))
          (ute:tween
           :to (((eon::screen-transition-fade-progress transition-fade)
                 (screen-transition-mosaic-progress transition-mosaic))
                (1.0 1.0))
           :duration 0.25 :ease #'ute:linear-inout))))
  #.(subst :from :to (subst 'example-screen-transition-in 'example-screen-transition-out '#1#)))

(defun promise-transition-example-screen-with-mosaic (screen)
  (eon:promise-transition-screen screen 'example-screen-transition-out 'example-screen-transition-in))

(setf (fdefinition 'promise-transition-example-screen) (fdefinition 'promise-transition-example-screen-with-mosaic))
