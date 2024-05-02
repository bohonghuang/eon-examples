(in-package #:eon-examples)

(defstruct tiled-map-example-screen
  (camera (raylib:make-camera-2d
           :offset (raylib:vector2-scale
                    (raylib:make-vector2
                     :x (float +viewport-width+)
                     :y (float +viewport-height+))
                    0.5)
           :target (raylib:make-vector2 :x 576.0 :y 576.0)
           :zoom 1.0 :rotation 0.0)
   :type raylib:camera-2d)
  (renderer #'values :type eon:tiled-renderer)
  (group (eon:scene2d-construct (eon:scene2d-group)) :type eon:scene2d-group))

(defmethod eon:screen-render ((screen tiled-map-example-screen))
  (raylib:clear-background raylib:+black+)
  (raylib:with-mode-2d (tiled-map-example-screen-camera screen)
    (funcall (tiled-map-example-screen-renderer screen)))
  (eon:scene2d-draw-simple (tiled-map-example-screen-group screen)))

(defun tiled-map-example ()
  (let ((screen (make-tiled-map-example-screen)))
    (let* ((eon:*tiled-renderer-camera* (tiled-map-example-screen-camera screen))
           (*async-continuation-constructor* (async-special-variable-binder (eon:*tiled-renderer-camera*))))
      (async
        (eon.debug:with-popped-prompt "Loading..."
          (setf (tiled-map-example-screen-renderer screen)
                (eon:tiled-map-renderer (await (eon:promise-task (curry #'tiled:load-map (example-asset #P"MagicLand.tmx")))))))
        (await (ajoin
                (promise-transition-example-screen screen)
                (promise:with-promise (succeed)
                  (eon:add-game-loop-hook
                   (let* ((camera (tiled-map-example-screen-camera screen))
                          (target (raylib:camera-2d-target camera)))
                     (lambda (&aux (amount (* (eon:game-loop-delta-time) 128.0)))
                       (when (eon:key-down-p :up)
                         (decf (raylib:vector2-y target) amount))
                       (when (eon:key-down-p :down)
                         (incf (raylib:vector2-y target) amount))
                       (when (eon:key-down-p :left)
                         (decf (raylib:vector2-x target) amount))
                       (when (eon:key-down-p :right)
                         (incf (raylib:vector2-x target) amount))
                       (when (eon:key-down-p :x)
                         (incf (raylib:camera-2d-zoom camera) (eon:game-loop-delta-time)))
                       (when (eon:key-down-p :y)
                         (decf (raylib:camera-2d-zoom camera) (eon:game-loop-delta-time)))
                       (when (eon:key-pressed-p :b)
                         (succeed))))
                   :after #'not))))))))

(pushnew 'tiled-map-example *examples*)
