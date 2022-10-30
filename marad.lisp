; marad.lisp - the main game code
;
;   "... In ancient times it [Marad] was on the canal, Abgal, running
;   between Babylon and Isin. The city's main temple, a ziggurat, is
;   E-igi-kalama (House which is the eye of the land)"
;     -- https://en.wikipedia.org/wiki/Marad

(in-package :marad)

(block nil (setq *random-state* (make-random-state t)) (return))

; TODO probably can ask SDL what the screen size is and then ...
(defconstant +game-width+ 1280)
(defconstant +game-height+ 800)

;(defconstant +border-x+ 128)
;(defconstant +border-y+ 80)

; the graph (background art) extends a bit beyond the window boundaries
(defconstant +graph-more+ 128)
(defconstant +graph-less+ (- (/ +graph-more+ 2)))
(defconstant +graph-line-width+ 4)
(defconstant +node-min-size+ 11)
(defconstant +node-max-size+ 83)

; optional adjustments for from where lines or such get drawn from (the
; background graph is drawn outside the window bounds)
(defparameter *offset-x* 0)
(defparameter *offset-y* 0)

(defconstant +node-count+ 128)

(defconstant +sdl-delay+ 128)   ; do not burn up the CPU too much

; bag of holding for "world" objects or whatever
(defstruct (app (:type vector))
  rend      ; SDL renderer
  graph     ; background graph art thing
  slope     ; a slope function for the graph
  )

(defun draw-line (renderer x1 y1 x2 y2)
  (sdl2:render-draw-line renderer
                         (+ *offset-x* x1) (+ *offset-y* y1)
                         (+ *offset-x* x2) (+ *offset-y* y2)))

(defun draw-rect (renderer x1 y1 width height)
  (sdl2:render-draw-rect
    renderer (sdl2:make-rect
               (+ *offset-x* x1) (+ *offset-y* y1)
               width height)))

(defun fill-rect (renderer x1 y1 width height)
  (sdl2:render-fill-rect
    renderer (sdl2:make-rect
               (+ *offset-x* x1) (+ *offset-y* y1)
               width height)))

(defun line-hori (renderer width x1 y1 length)
  (let* ((adjust (truncate (/ width 2))) (x2 (+ x1 length adjust)))
    (loop for w from 0 below width do
          (draw-line renderer x1 (+ y1 w) x2 (+ y1 w)))))

(defun line-vert (renderer width x1 y1 length)
  (let* ((adjust (truncate (/ width 2))) (y2 (+ y1 length adjust)))
    (loop for w from 0 below width do
          (draw-line renderer (+ x1 w) y1 (+ x1 w) y2))))

; diagonal lines are problematic lacking SDL GFX so instead we use
(defun squareline (renderer width x1 y1 x2 y2 &aux (mulx 1) (muly 1))
  (when (> x1 x2) (setf mulx -1))
  (when (> y1 y2) (setf muly -1))
  (loop until (and (= x1 x2) (= y1 y2)) do
        (let ((dx (abs (- x2 x1))) (dy (abs (- y2 y1))))
          (if (> dx dy)
            (let ((dx (* mulx dx )))
              (line-hori renderer width x1 y1 dx)
              (incf x1 dx))
            (let ((dy (* muly dy )))
              (line-vert renderer width x1 y1 dy)
              (incf y1 dy))))))

; linear limit of inputs along the axis x1..x2 to the range y1..y2
(defun new-clamped-slope (x1 y1 x2 y2)
  (let* ((m (/ (- y2 y1) (- x2 x1))) (b (- y1 (* m x1))))
    (lambda (x)
      (let ((y (+ (* m x) b)))
        (if (> y y2) y2 (if (< y y1) y1 y))))))

(defun update-graph (renderer app)
  (sdl2:set-render-draw-color renderer 0 0 0 255) ; graph skari
  (let ((*offset-x* +graph-less+) (*offset-y* +graph-less+)
        (graph (app-graph app)) (slopefn (app-slope app)))
    (loop for n in (nodeset-network graph) do
          (when-let (m (snode-next n))
            (squareline renderer +graph-line-width+
                        (snode-xx n) (snode-yy n)
                        (snode-xx m) (snode-yy m)))
          (let ((size (round (funcall slopefn (snode-population n)))))
            (fill-rect renderer
                       (- (snode-xx n) 0) (- (snode-yy n) 0)
                       size size)))))

(defun update (app)
  (let ((renderer (app-rend app)))
    (sdl2:set-render-draw-color renderer 192 192 192 255) ; bg skari
    (sdl2:render-clear renderer)
    (update-graph renderer app)
    (sdl2:render-present renderer)))

(defun eventually (app)
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
; TODO do we need mouse motion?
;   (:mousemotion
;     (:x x :y y :xrel xrel :yrel yrel :state state)
;     )
; TODO steal a mouse and see other buttons get other button numbers
    (:mousebuttondown
      (:x x :y y :button button)
      (format t "CLICK ~a,~a (~a)~&" x y button))
    (:idle () (update app) (sdl2:delay +sdl-delay+))
    (:quit () t)))

(defun world (&aux (app (make-app)))
  (let ((graph (new-graph (+ +game-width+ +graph-more+)
                          (+ +game-height+ +graph-more+) +node-count+)))
    (setf (app-graph app) graph
          (app-slope app) (new-clamped-slope
                            (nodeset-pop-min graph) +node-min-size+
                            (nodeset-pop-max graph) +node-max-size+)))
  app)

(defun main (&aux (app (world)))
  (sdl2:with-init
    (:everything)
    (sdl2:with-window
      (win :title "Marad" :w +game-width+ :h +game-height+ :flags '(:shown))
      (sdl2:with-renderer
        (renderer win :flags '(:accelerated))
        (setf (app-rend app) renderer)
        (update app)
        (eventually app)))))

; NOTE some LISP implementations on some operating systems require that
; SLD2 be run from the main thread, hence make-this-thread-main. if that
; causes a problem you can probably call directly to #'main
(defun _start ()
  (sdl2:make-this-thread-main #'main)
  (fresh-line))
