(in-package #:eon-examples)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +select-box-example-fruits+
      '("Apple"
        "Banana"
        "Orange"
        "Strawberry"
        "Grapes"
        "Watermelon"
        "Pineapple"
        "Mango"
        "Pear"
        "Kiwi")
    :test #'equal))

(eon:define-scene2d-constructed select-box-example-screen-cell
    (scene2d-screen-cell
     :child (eon:scene2d-window
             :child (eon:scene2d-margin
                     :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                     :child (eon:select-box
                             :name select-box
                             :dimensions (2 T)
                             :entries (mapcar
                                       (lambda (string)
                                         (eon:scene2d-construct
                                          (eon:scene2d-margin
                                           :top 1.0 :bottom 1.0 :left 1.0 :right 1.0
                                           :child (eon:scene2d-max-cell
                                                   :size (64.0 0.0)
                                                   :child (eon:scene2d-label :string string)))))
                                       +select-box-example-fruits+))))))

(defstruct select-box-example-screen
  (cell (make-select-box-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen select-box-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (select-box-example-screen-cell screen)))

(defun select-box-example ()
  (let* ((screen (make-select-box-example-screen))
         (cell (select-box-example-screen-cell screen))
         (select-box (select-box-example-screen-cell-select-box cell)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (loop :for index := (await (eon:select-box-promise-index select-box (or index 0)))
             :while index
             :do (format t "You selected ~A.~%" (nth index +select-box-example-fruits+)))))))

(pushnew 'select-box-example *examples*)
