(defsystem eon-examples
  :version "0.1.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "A collection of examples introducing the functionalities of the EON game framework."
  :homepage "https://github.com/bohonghuang/eon-examples"
  :bug-tracker "https://github.com/bohonghuang/eon-examples/issues"
  :source-control (:git "https://github.com/bohonghuang/eon-examples.git")
  :depends-on (#:alexandria #:eon #:eon.debug)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "scene2d-image")
                             (:file "scene2d-window")
                             (:file "scene2d-label")
                             (:file "scene2d-layout")
                             (:file "dialog-box")
                             (:file "select-box")
                             (:file "input-field")
                             (:file "parallax-background")
                             (:file "tiled-map")
                             (:file "basic-3d-scene")
                             (:file "particle-system")
                             (:file "bgm-sfx")
                             (:file "screen-transition")
                             (:file "logo")))))
