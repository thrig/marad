; marad.lisp - the main game code
;
;   "... In ancient times it [Marad] was on the canal, Abgal, running
;   between Babylon and Isin. The city's main temple, a ziggurat, is
;   E-igi-kalama (House which is the eye of the land)"
;     -- https://en.wikipedia.org/wiki/Marad
;
; ... but I cannot think of any game to make from it! oh well

(in-package :marad)

(block nil (setq *random-state* (make-random-state t)) (return))

(defconstant +game-width+ 1280)
(defconstant +game-height+ 800)

(defconstant +border-x+ 128)
(defconstant +border-y+ 80)
(defconstant +offset-x+ (/ +border-x+ 2))
(defconstant +offset-y+ (/ +border-y+ 2))

(defconstant +node-count+ 128)

(defconstant +sdl-delay+ 128)   ; do not burn up the CPU too much

(defun rclear (renderer)
  (sdl2:set-render-draw-color renderer 192 192 192 255) ; bg color
  (sdl2:render-clear renderer))

(defun draw-line (renderer x1 y1 x2 y2)
  (sdl2:render-draw-line renderer
                         (+ +offset-x+ x1) (+ +offset-y+ y1)
                         (+ +offset-x+ x2) (+ +offset-y+ y2)))

(defun draw-rect (renderer x1 y1 width height)
  (sdl2:render-draw-rect
    renderer (sdl2:make-rect
               (+ +offset-x+ x1) (+ +offset-y+ y1)
               width height)))

(defun fill-rect (renderer x1 y1 width height)
  (sdl2:render-fill-rect
    renderer (sdl2:make-rect
               (+ +offset-x+ x1) (+ +offset-y+ y1)
               width height)))

(defun line-hori (renderer width x1 y1 length)
  (let* ((adjust (truncate (/ width 2))) (x2 (+ x1 length adjust)))
    (loop for w from 0 below width do
          (draw-line renderer x1 (+ y1 w) x2 (+ y1 w)))))

(defun line-vert (renderer width x1 y1 length)
  (let* ((adjust (truncate (/ width 2))) (y2 (+ y1 length adjust)))
    (loop for w from 0 below width do
          (draw-line renderer (+ x1 w) y1 (+ x1 w) y2))))

(defun squareline (renderer width x1 y1 x2 y2
                  &aux (max-loop 20) (mulx 1) (muly 1))
  (when (> x1 x2) (setf mulx -1))
  (when (> y1 y2) (setf muly -1))
  (loop until (and (= x1 x2) (= y1 y2)) do
        (when (zerop (decf max-loop)) (return))
        (let ((dx (abs (- x2 x1))) (dy (abs (- y2 y1))))
          (if (> dx dy)
            (let ((dx (* mulx dx )))
              (line-hori renderer width x1 y1 dx)
              (incf x1 dx))
            (let ((dy (* muly dy )))
              (line-vert renderer width x1 y1 dy)
              (incf y1 dy))))))

(defun update (renderer graph)
  ;(format t "~&-- update")
  (rclear renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (loop for n in (nodeset-network graph) do
        (when-let (m (snode-next n))
          (squareline renderer 5 (snode-xx n) (snode-yy n) (snode-xx m) (snode-yy m)))
        (fill-rect renderer (- (snode-xx n) 0) (- (snode-yy n) 0) 15 15))
  (sdl2:render-present renderer))

(defun event-loop (renderer graph)
  (sdl2:with-event-loop
    (:method :poll)
    (:keyup
      (:keysym keysym)
      (let ((scancode (sdl2:scancode-value keysym))
            (sym (sdl2:sym-value keysym))
            (mod-value (sdl2:mod-value keysym)))
        (cond
          ((sdl2:scancode= scancode :scancode-q) (sdl2:push-event :quit)))
        (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)))
    (:mousemotion
      (:x x :y y :xrel xrel :yrel yrel :state state)
      ; TODO if this is "close enough" to a node light the node up?
      ;(format t "Mouse motion abs(rel): ~a,~a (~a,~a) state ~a~%" x y xrel yrel state))
      (:mousebuttondown
        (:x x :y y :button button)
        (format t "CLICK ~a,~a (~a)~&" x y button))
      (:idle () (update renderer graph) (sdl2:delay +sdl-delay+))
      (:quit () t)))

(defun main (&aux (graph (new-graph (- +game-width+ +border-x+)
                                    (- +game-height+ +border-y+)
                                    +node-count+)))
  (sdl2:with-init
    (:everything)
    (sdl2:with-window
      (win :title "Marad" :w +game-width+ :h +game-height+ :flags '(:shown))
      ; really bad with CWM, messes up the usual four terminals on exit
      ;(sdl2:set-window-fullscreen win t)
      (sdl2:with-renderer
        (renderer win :flags '(:accelerated))
        (update renderer graph)
        (event-loop renderer graph)))))

; NOTE some implementations on some OS require that SLD2 be run from the
; main thread, hence the make-this-thread-main thing. if that causes a
; problem you can probably call directly to #'main from
; save-lisp-and-die?
(defun _start ()
  (sdl2:make-this-thread-main #'main)
  (fresh-line))
