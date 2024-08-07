(in-package #:eon-examples)

(eon:define-scene2d-constructed dialog-box-example-screen-cell ()
  (scene2d-screen-cell
   :child (eon:scene2d-window
           :child (eon:scene2d-margin
                   :left 6.0 :right 6.0 :top 2.0 :bottom 2.0
                   :child (eon:dialog-box
                           :name dialog-box
                           :string "Welcome to EON!")))))

(defstruct dialog-box-example-screen
  (cell (make-dialog-box-example-screen-cell) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen dialog-box-example-screen))
  (raylib:clear-background raylib:+raywhite+)
  (eon:scene2d-draw-simple (dialog-box-example-screen-cell screen)))

(defun dialog-box-example ()
  (let* ((screen (make-dialog-box-example-screen))
         (cell (dialog-box-example-screen-cell screen))
         (dialog-box (dialog-box-example-screen-cell-dialog-box cell)))
    (eon:scene2d-layout cell)
    (ajoin
     (promise-transition-example-screen screen)
     (async
       (await (eon:dialog-box-promise-display-confirm dialog-box :next))
       (setf (eon:dialog-box-string dialog-box) "Common Lisp (CL) is a dialect of the Lisp programming language, published in American National Standards Institute (ANSI) standard document ANSI INCITS 226-1994 (S2018) (formerly X3.226-1994 (R1999)).")
       (await (eon:dialog-box-promise-display-confirm dialog-box))))))

(pushnew 'dialog-box-example *examples*)
