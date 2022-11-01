(asdf:defsystem
  :marad
  :description "an Autumn 2022 Lisp Game Jam game"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:sdl2)
  :components ((:static-file "README")
               (:static-file "RULES")
               (:static-file "LICENSE")
               (:static-file "image/README")
               (:static-file "image/black-bishop.bmp")
               (:static-file "image/black-king.bmp")
               (:static-file "image/black-pawn.bmp")
               (:static-file "image/white-bishop.bmp")
               (:static-file "image/white-king.bmp")
               (:static-file "image/white-pawn.bmp")
               (:file "package")
               (:file "graph" :depends-on ("package"))
               (:file "board" :depends-on ("package"))
               (:file "marad" :depends-on ("package" "graph" "board"))))
