;   <Uncle Somf> How are we doing, $outlaw_name?
;   <$outlaw_name> The same as always, Uncle!
;   <Uncle Somf> That bad, huh?
;     -- The Battle for Wesnoth. David White et al. 2005.

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
               (:static-file "image/LICENSE.whitrabt")
               (:static-file "image/README")
               (:static-file "image/whitrabt.ttf")
               (:static-file "image/0.bmp")
               (:static-file "image/1.bmp")
               (:static-file "image/2.bmp")
               (:static-file "image/3.bmp")
               (:static-file "image/4.bmp")
               (:static-file "image/5.bmp")
               (:static-file "image/6.bmp")
               (:static-file "image/7.bmp")
               (:static-file "image/8.bmp")
               (:static-file "image/9.bmp")
               (:static-file "image/black.1.bmp")
               (:static-file "image/black.2.bmp")
               (:static-file "image/black.3.bmp")
               (:static-file "image/white.1.bmp")
               (:static-file "image/white.2.bmp")
               (:static-file "image/white.3.bmp")
               (:file "package")
               (:file "graph" :depends-on ("package"))
               (:file "board" :depends-on ("package"))
               (:file "marad" :depends-on ("package" "graph" "board"))))
