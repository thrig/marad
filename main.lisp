(in-package :marad)

(defun main ()
  (sdl2:with-init
    (:everything)
    (sdl2:with-window
      (win :title "Marad" :w 1280 :h 800 :flags '(:shown))
      ; really bad with CWM, messes up the usual four terminals on exit
      ;(sdl2:set-window-fullscreen win t)
      (sdl2:with-renderer
        (renderer win :flags '(:accelerated))
        (rclear renderer)
        (sdl2:render-present renderer)
        (sdl2:with-event-loop
          (:method :poll)
          (:keyup
            (:keysym keysym)
            (let ((scc (sdl2:scancode-value keysym))
                  ; (sym (sdl2:sym-value keysym))
                  ; (mod-value (sdl2:mod-value keysym))
                  )
              (cond
                ((sdl2:scancode= scc :scancode-q) (sdl2:push-event :quit))
                ;(t (sdl2:push-event :quit))
                )
              )
            )
          (:idle ()
                 ; presumably animations or such might happen here?
                 ; one can dream
                 (sdl2:delay 100)
                 )
          (:quit () t) ; this is pretty important to have
          )))))

; NOTE some implementations on some OS require that SLD2 be run from the
; main thread, hence the make-this-thread-man thing. if that causes a
; problem you can probably call directly to #'main from
; save-lisp-and-die?
(defun _start ()
  (sdl2:make-this-thread-main #'main)
  (fresh-line))
