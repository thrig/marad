(asdf:defsystem
  :marad
  :description "an Autumn 2022 Lisp Game Jam game"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:sdl2)
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "graph" :depends-on ("package"))
               (:file "board" :depends-on ("package"))
               (:file "marad" :depends-on ("package" "graph" "board"))))
