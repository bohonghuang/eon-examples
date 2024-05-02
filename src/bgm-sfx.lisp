(in-package #:eon-examples)

(eon:define-scene2d-constructed bgm-sfx-example-entry
    (eon:scene2d-box
     :name box
     :orientation :horizontal
     :children ((eon:scene2d-max-cell
                 :size (64.0 0.0)
                 :alignment (:start :center)
                 :child (eon:scene2d-label :name key-label :string ""))
                (eon:scene2d-max-cell
                 :size (64.0 0.0)
                 :alignment (:end :center)
                 :child (eon:scene2d-label :name value-label :string ""))))
  (:constructor (&key key value)
      (let ((entry (%make-bgm-sfx-example-entry)))
        (setf (eon:scene2d-label-string (bgm-sfx-example-entry-key-label entry)) key
              (bgm-sfx-example-entry-playing-p entry) value)
        entry)))

(defun bgm-sfx-example-entry-playing-p (entry)
  (string= (eon:scene2d-label-string (bgm-sfx-example-entry-value-label entry)) "PLAYING"))

(defun (setf bgm-sfx-example-entry-playing-p) (value entry)
  (setf (eon:scene2d-label-string (bgm-sfx-example-entry-value-label entry)) (if value "PLAYING" "STOPPED")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (eon:define-scene2d-default-construct-form bgm-sfx-example-entry (key value)))

(eon:define-scene2d-constructed bgm-sfx-example-screen-cell
    (scene2d-screen-cell
     :child (eon:scene2d-window
             :child (eon:select-box
                     :name select-box
                     :dimensions (1 T)
                     :entries ((bgm-sfx-example-entry :key "BGM" :value nil)
                               (bgm-sfx-example-entry :key "SFX" :value nil))))))

(defstruct bgm-sfx-example-screen
  (cell (make-bgm-sfx-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen bgm-sfx-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (bgm-sfx-example-screen-cell screen)))

(defun bgm-sfx-example ()
  (let* ((screen (make-bgm-sfx-example-screen))
         (cell (bgm-sfx-example-screen-cell screen))
         (select-box (bgm-sfx-example-screen-cell-select-box cell))
         (bgm (eon:load-asset 'raylib:music (example-asset #P"country.mp3")))
         (sfx (eon:load-asset 'raylib:sound (example-asset #P"boom.wav"))))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :for index := (await (eon:select-box-promise-index select-box (or index 0)))
             :while index
             :do (ecase index
                   (0
                    (if (eon:audio-playing-p bgm)
                        (progn
                          (await (eon:promise-fade-audio bgm 0.0))
                          (eon:stop-audio bgm))
                        (progn
                          (setf (eon:audio-volume bgm) 1.0)
                          (eon:play-audio bgm)))
                    (setf (bgm-sfx-example-entry-playing-p (first (eon:select-box-children select-box))) (eon:audio-playing-p bgm)))
                   (1
                    (when (eon:audio-playing-p sfx)
                      (eon:stop-audio sfx))
                    (async
                      (let ((entry (second (eon:select-box-children select-box))))
                        (setf (bgm-sfx-example-entry-playing-p entry) t)
                        (await (eon:promise-play-audio sfx))
                        (setf (bgm-sfx-example-entry-playing-p entry) nil)))))
             :finally
                (when (eon:audio-playing-p bgm)
                  (eon:stop-audio bgm)))))))

(pushnew 'bgm-sfx-example *examples*)
