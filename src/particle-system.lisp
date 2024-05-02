(in-package #:eon-examples)

(defstruct particle-system-example-screen
  (emitter-index 0 :type non-negative-fixnum)
  (emitters (list (eon:make-scene3d-particle-emitter
                   :updater (eon:scene3d-particle-emitter-billboard-updater)
                   :renderer (eon:particle-3d-cube-renderer
                              0.25 (eon:particle-3d-interpolate-color-over-age raylib:+red+ (raylib:fade raylib:+orange+ 0.0)))
                   :rate 60.0)
                  (eon:make-scene3d-particle-emitter
                   :updater (eon:scene3d-particle-emitter-billboard-updater :lifetime 0.5)
                   :renderer (eon:particle-3d-scene3d-billboard-renderer
                              (eon:ensure-scene3d-node
                               (eon:load-asset 'raylib:texture (example-asset #P"lisp-logo.png"))
                               :scale (raylib:vector3-scale (raylib:vector3-one) 0.01)
                               :up (raylib:camera-3d-up eon:*scene3d-camera*))
                              :scale (eon:particle-3d-interpolate-vector3-over-age
                                      (raylib:vector3-zero) (raylib:vector3-one) #'ute:quad-out)
                              :color (eon:particle-3d-interpolate-color-over-age
                                      raylib:+white+ (raylib:fade raylib:+blank+ 0.0) #'ute:quad-in)
                              :rotation (eon:particle-3d-interpolate-quaternion-over-age
                                         (raylib:quaternion-from-euler 0.0 0.0 0.0)
                                         (raylib:quaternion-from-euler 0.0 0.0 (coerce pi 'single-float)))
                              :epsilon 0.01)
                   :rate 60.0)
                  (eon:make-scene3d-particle-emitter
                   :updater (lambda (origin)
                              (let ((position (raylib:make-vector3))
                                    (velocity (raylib:make-vector3))
                                    (gravity (raylib:make-vector3 :x 0.0 :y -20.0 :z 0.0))
                                    (rotation (raylib:quaternion-identity))
                                    (velocity-2d (raylib:vector2-scale (raylib:vector2-one) 2.0)))
                                (flet ((generate-position ()
                                         (setf (raylib:vector3-x position) (+ (raylib:vector3-x origin) (- 0.5 (random 1.0)))
                                               (raylib:vector3-y position) (raylib:vector3-y origin)
                                               (raylib:vector3-z position) (+ (raylib:vector3-z origin) (- 0.5 (random 1.0))))
                                         position)
                                       (generate-velocity ()
                                         (raylib:%vector2-rotate (& velocity-2d) (& velocity-2d) (random (coerce pi 'single-float)))
                                         (setf (raylib:vector3-x velocity) (raylib:vector2-x velocity-2d)
                                               (raylib:vector3-y velocity) (+ 7.5 (random 7.5))
                                               (raylib:vector3-z velocity) (raylib:vector2-y velocity-2d))
                                         velocity))
                                  (lambda (particle)
                                    (if (zerop (eon:particle-3d-age particle))
                                        (setf (eon:particle-3d-position particle) (generate-position)
                                              (eon:particle-3d-velocity particle) (generate-velocity)
                                              (eon:particle-3d-acceleration particle) gravity
                                              (eon:particle-3d-rotation particle) rotation
                                              (eon:particle-3d-rotation-velocity particle) rotation
                                              (eon:particle-3d-rotation-acceleration particle) rotation
                                              (eon:particle-3d-age particle) 0.0
                                              (eon:particle-3d-lifetime particle) 2.0)
                                        (let ((position (eon:particle-3d-position particle))
                                              (velocity (eon:particle-3d-velocity particle)))
                                          (when (minusp (raylib:vector3-y position))
                                            (setf (raylib:vector3-y position) 0.0
                                                  (raylib:vector3-y velocity) (* -0.5 (raylib:vector3-y velocity))))
                                          (eon::particle-3d-update-motion particle)))))))
                   :renderer (eon:particle-3d-cube-renderer
                              0.25
                              (eon:particle-3d-interpolate-color-over-age
                               raylib:+red+ raylib:+blank+ #'ute:expo-in))
                   :rate 60.0))
   :type list)
  (shader (eon:load-asset 'raylib:shader (example-asset #P"alpha-discard.fs")) :type raylib:shader)
  (group (eon:scene2d-construct (eon:scene2d-group)) :type eon:scene2d-group)
  (camera eon:*scene3d-camera* :type raylib:camera-3d))

(defmethod eon:screen-render ((screen particle-system-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (let ((eon:*scene3d-camera* (particle-system-example-screen-camera screen)))
    (raylib:update-camera eon:*scene3d-camera* #.(cffi:foreign-enum-value 'raylib:camera-mode :orbital))
    (raylib:with-shader-mode (particle-system-example-screen-shader screen)
      (raylib:with-mode-3d eon:*scene3d-camera*
        (raylib:draw-grid 100 1.0)
        (let ((emitter (nth (particle-system-example-screen-emitter-index screen)
                            (particle-system-example-screen-emitters screen))))
          (eon:scene3d-draw-simple emitter))))
    (eon:scene2d-draw-simple (particle-system-example-screen-group screen))))

(defun particle-system-example ()
  (let* ((eon:*scene3d-camera*
           (raylib:make-camera-3d
            :position (raylib:make-vector3 :x 0.0 :y 10.0 :z 10.0)
            :target (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0)
            :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0)
            :fovy 30.0 :projection #.(cffi:foreign-enum-value 'raylib:camera-projection :perspective)))
         (screen (make-particle-system-example-screen))
         (eon.debug:*debug-window-group* (particle-system-example-screen-group screen)))
    (with-accessors ((emitter-index particle-system-example-screen-emitter-index)
                     (emitters particle-system-example-screen-emitters))
        screen
      (ajoin
       (promise-transition-example-screen screen)
       (async
         (loop
           (case (eon.debug:with-popped-prompt (format nil "< ~A: ~D >" 'emitter-index emitter-index)
                   (await (eon:promise-pressed-key)))
             (:left (setf emitter-index (mod (1- emitter-index) (length emitters))))
             (:right (setf emitter-index (mod (1+ emitter-index) (length emitters))))
             (:b (return)))))))))

(pushnew 'particle-system-example *examples*)
