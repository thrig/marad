(asdf:defsystem
  #:marad
  :description "an Autumn 2022 Lisp Game Jam game"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:sdl2)
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "main" :depends-on ("package"))))
