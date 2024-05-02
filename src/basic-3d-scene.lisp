(in-package #:eon-examples)

(defstruct (basic-3d-scene (:constructor %make-basic-3d-scene))
  (objects nil :type list)
  (camera (raylib:make-camera-3d
           :position (raylib:make-vector3 :x 0.0 :y 5.0 :z 5.0)
           :target (raylib:make-vector3 :x 0.0 :y 0.0 :z -0.5)
           :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0)
           :fovy 30.0 :projection #.(cffi:foreign-enum-value 'raylib:camera-projection :perspective))
   :type raylib:camera-3d)
  (shadow (eon:make-shadow-map-renderer) :type eon:shadow-map-renderer)
  (shader (eon:load-asset 'raylib:shader (example-asset #P"basic-3d-scene")) :type raylib:shader)
  (shader-uniforms (make-basic-3d-scene-shader-uniforms) :type cobj:cobject))

(defvar *basic-3d-scene-shader-uniforms-shadow-map*)

(eon:define-shaderable-uniforms basic-3d-scene
  ("colDiffuse" raylib:+white+ :type raylib:color)
  ("colEmission" raylib:+black+ :type raylib:color)
  ("colAmbient" raylib:+lightgray+ :type raylib:color)
  ("colSpecular" raylib:+black+ :type raylib:color)
  ("colLight" raylib:+white+ :type raylib:color)
  ("lightVector" (raylib:make-vector3 :x 0.57735026 :y -0.57735026 :z -0.57735026) :type raylib:vector3)
  ("shadowLightMatrix" (raylib:make-matrix) :type raylib:matrix)
  ("shadowIntensity" (/ 3.0) :type single-float)
  ("shadowMap" *basic-3d-scene-shader-uniforms-shadow-map* :type raylib:texture))

(cobj:define-global-cobject +shadow-light-camera-default+
    (raylib:make-camera-3d :position (raylib:make-vector3 :x -2.0 :y 6.0 :z 2.0) :target (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0) :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0) :fovy 5.0 :projection (cffi:foreign-enum-value 'raylib:camera-projection :orthographic)))

(defun make-basic-3d-scene (&rest args)
  (let* ((shadow (eon:make-shadow-map-renderer :camera +shadow-light-camera-default+))
         (scene (let ((*basic-3d-scene-shader-uniforms-shadow-map*
                        (eon:shadow-map-renderer-texture shadow)))
                  (apply #'%make-basic-3d-scene :shadow shadow args))))
    (initialize-basic-3d-scene-shader-uniforms scene)
    (setf (eon:shadow-map-renderer-matrix shadow) (basic-3d-scene-shadow-light-matrix scene))
    scene))

(defun basic-3d-scene-draw (scene)
  (let ((renderer (basic-3d-scene-shadow scene))
        (camera (basic-3d-scene-camera scene)))
    (flet ((render-objects ()
             (let ((eon:*scene3d-camera* camera))
               (dolist (object (basic-3d-scene-objects scene))
                 (eon:scene3d-draw-simple object)))))
      (eon:shadow-map-renderer-render renderer #'render-objects)
      (raylib:with-mode-3d camera
        (raylib:with-shader-mode (basic-3d-scene-shader scene)
          (update-basic-3d-scene-shader-uniforms scene)
          (rlgl:disable-depth-mask)
          (raylib:draw-plane
           (raylib:vector3-zero)
           (raylib:vector2-scale (raylib:vector2-one) 10.0)
           raylib:+raywhite+)
          (rlgl:draw-render-batch-active)
          (rlgl:enable-depth-mask)
          (let ((shadow-intensity (basic-3d-scene-shadow-intensity scene)))
            (setf (basic-3d-scene-shadow-intensity scene) 0.0)
            (update-basic-3d-scene-shader-uniforms scene)
            (setf (basic-3d-scene-shadow-intensity scene) shadow-intensity))
          (render-objects))
        (raylib:draw-grid 100 1.0)))))

(defstruct basic-3d-scene-example-screen
  (scene (make-basic-3d-scene) :type basic-3d-scene))

(defmethod eon:screen-render ((screen basic-3d-scene-example-screen))
  (raylib:clear-background raylib:+lightgray+)
  (basic-3d-scene-draw (basic-3d-scene-example-screen-scene screen)))

(defstruct (scene3d-cube (:include eon:scene3d-layout)
                         (:constructor %make-scene3d-cube)))

(defmethod eon:scene3d-draw ((cube scene3d-cube) position origin scale rotation tint)
  (let ((min (raylib:bounding-box-min (scene3d-cube-bound cube)))
        (max (raylib:bounding-box-max (scene3d-cube-bound cube))))
    (rlgl:push-matrix)
    (eon::rlgl-apply-scene3d-draw-arguments position origin scale rotation tint)
    (raylib:draw-cube-v
     (raylib:vector3-scale (raylib:vector3-subtract max min) 0.5)
     (raylib:vector3-subtract max min)
     raylib:+red+)
    (raylib:draw-cube-wires-v
     (raylib:vector3-scale (raylib:vector3-subtract max min) 0.5)
     (raylib:vector3-subtract max min)
     (raylib:color-brightness raylib:+red+ -0.25))
    (rlgl:pop-matrix)))

(defun make-scene3d-cube (&rest args)
  (let ((cube (apply #'%make-scene3d-cube args)))
    (setf (raylib:bounding-box-min (scene3d-cube-bound cube)) (raylib:vector3-scale (raylib:vector3-one) -0.5)
          (raylib:bounding-box-max (scene3d-cube-bound cube)) (raylib:vector3-scale (raylib:vector3-one) 0.5)
          (raylib:vector3-x (scene3d-cube-origin cube)) 0.5
          (raylib:vector3-z (scene3d-cube-origin cube)) 0.5)
    cube))

(defun quaternion-euler-pitch (quaternion)
  (clet ((euler (foreign-alloca '(:struct raylib:vector3))))
    (raylib:%quaternion-to-euler euler (& quaternion))
    (-> euler raylib:y)))

(defun (setf quaternion-euler-pitch) (value quaternion)
  (clet ((euler (foreign-alloca '(:struct raylib:vector3))))
    (raylib:%quaternion-to-euler euler (& quaternion))
    (raylib:%quaternion-from-euler (& quaternion) (-> euler raylib:x) value (-> euler raylib:z))))

(defun basic-3d-scene-example ()
  (let* ((screen (make-basic-3d-scene-example-screen))
         (scene (basic-3d-scene-example-screen-scene screen))
         (cube (make-scene3d-cube :position (raylib:make-vector3 :x -1.0 :y 0.0 :x 0.0)))
         (model (eon:load-asset 'raylib:model (example-asset #P"robot.glb")))
         (object (eon:make-scene3d-container
                  :content model
                  :position (raylib:make-vector3 :x 1.0 :y 0.0 :x 0.0)
                  :scale (raylib:vector3-scale (raylib:vector3-one) 0.5)))
         (tween (ute:timeline
                 (:sequence
                  (:to (((quaternion-euler-pitch (scene3d-cube-rotation cube))) (0.0)))
                  (:to (((quaternion-euler-pitch (scene3d-cube-rotation cube))) ((* 0.5 (coerce pi 'single-float))))
                   :duration 2.0
                   :ease #'ute:linear-inout)
                  :repeat t))))
    (loop :with materials := (raylib:model-materials model)
          :for i :below (raylib:model-material-count model)
          :do (setf (raylib:material-shader (cobj:cref materials i)) (basic-3d-scene-shader scene)))
    (let* ((animations (eon:load-asset 'raylib:model-animations (example-asset #P"robot.glb")))
           (animation (cobj:caref animations 3))
           (animation-frame-count (raylib:model-animation-frame-count animation))
           (animation-frame-index 0))
      (eon:add-game-loop-hook
       (lambda ()
         (raylib:update-model-animation model animation (setf animation-frame-index (mod (1+ animation-frame-index) animation-frame-count)))
         (ute::timeline-startedp tween))
       :after #'identity))
    (push cube (basic-3d-scene-objects scene))
    (push object (basic-3d-scene-objects scene))
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :initially (ute:start tween)
             :until (eq (await (eon:promise-pressed-key)) :b)
             :finally (ute:kill tween))))))

(pushnew 'basic-3d-scene-example *examples*)
