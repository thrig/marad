; marad.lisp - the main game code
;
;   "... In ancient times it [Marad] was on the canal, Abgal, running
;   between Babylon and Isin. The city's main temple, a ziggurat, is
;   E-igi-kalama (House which is the eye of the land)"
;     -- https://en.wikipedia.org/wiki/Marad

(in-package :marad)

(defconstant +game-width+ 1280)
(defconstant +game-height+ 800)
(defconstant +border-x+ 128)
(defconstant +border-y+ 80)

(block nil (setq *random-state* (make-random-state t)) (return))

(defun rclear (renderer)
  (sdl2:set-render-draw-color renderer 192 192 192 255)
  (sdl2:render-clear renderer))

; TODO at some angles this still draws very thin
(defun thick-line (renderer width x1 y1 x2 y2)
  (loop for w from 1 to width do
    (sdl2:render-draw-line renderer (+ x1 w) y1 (+ x2 w) y2)))

(defun main (&aux (graph (new-graph (- +game-width+ +border-x+)
                                    (- +game-height+ +border-y+) 100)))
  (sdl2:with-init
    (:everything)
    (sdl2:with-window
      (win :title "Marad" :w +game-width+ :h +game-height+ :flags '(:shown))
      ; really bad with CWM, messes up the usual four terminals on exit
      ;(sdl2:set-window-fullscreen win t)
      (sdl2:with-renderer
        (renderer win :flags '(:accelerated))
        (rclear renderer)
        (sdl2:set-render-draw-color renderer 0 0 0 255)
        (loop for n in (nodeset-network graph) do
              (when-let (m (snode-next n))
                (thick-line renderer 8
                            (+ (/ +border-x+ 2) (snode-xx n)) (+ (/ +border-y+ 2) (snode-yy n))
                            (+ (/ +border-x+ 2) (snode-xx m)) (+ (/ +border-y+ 2) (snode-yy m)))
                )
              )
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
