; marad.lisp - the main game code
;
;   "... In ancient times it [Marad] was on the canal, Abgal, running
;   between Babylon and Isin. The city's main temple, a ziggurat, is
;   E-igi-kalama (House which is the eye of the land)"
;     -- https://en.wikipedia.org/wiki/Marad

(in-package :marad)

(block nil (setq *random-state* (make-random-state t)) (return))

; TODO ask SDL what the screen size is and then derive a bunch of the
; following constants from that
(defconstant +game-width+ 1280)
(defconstant +game-height+ 800)

; for a NxN board
(defconstant +board-cells+ 9)
(defconstant +middle-cell+ 4)
; don't let the board take up all the window (TODO probably the board
; needs a border drawn around it)
(defconstant +border-x+ 64)
(defconstant +border-y+ 40)
(defconstant +border-offx+ (/ +border-x+ 2))
(defconstant +border-offy+ (/ +border-y+ 2))
(defconstant +board-line-width+ 2)

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
  cellwidth ; how big the board is
  cellsize  ; how big a board cell is
  cellxoff  ; where to start drawing the board from
  cellyoff
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
(defun square-line (renderer width x1 y1 x2 y2 &aux (mulx 1) (muly 1))
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

; how big are the cells? and also the x,y offsets for the origin
(defun square-size (width height cell-count)
  (let* ((cell-size (truncate (/ (min width height) cell-count)))
         (total-cell-size (* cell-size cell-count)))
    (values total-cell-size
            cell-size
            (truncate (/ (- width total-cell-size) 2))
            (truncate (/ (- height total-cell-size) 2)))))

(defun update-graph (renderer app)
  (sdl2:set-render-draw-color renderer 192 192 192 255) ; graph node skari
  (let ((*offset-x* +graph-less+) (*offset-y* +graph-less+)
        (graph (app-graph app)) (slopefn (app-slope app)))
    (loop for n in (nodeset-network graph) do
          (when-let (m (snode-next n))
            (square-line renderer +graph-line-width+
                        (snode-xx n) (snode-yy n)
                        (snode-xx m) (snode-yy m)))
          (let ((size (round (funcall slopefn (snode-population n)))))
            (fill-rect renderer
                       (- (snode-xx n) 0) (- (snode-yy n) 0)
                       size size)))))

(defun update-board (renderer app)
  ; NOTE how to use blend is somewhat not documented
  ;   https://github.com/lispgames/cl-sdl2/issues/153
  (sdl2:set-render-draw-blend-mode renderer sdl2-ffi:+sdl-blendmode-blend+)
  (sdl2:set-render-draw-color renderer 255 255 255 240) ; board skari
  (let* ((*offset-x* (+ +border-offx+ (app-cellxoff app)))
         (*offset-y* (+ +border-offy+ (app-cellyoff app)))
         (boardsize (app-cellwidth app))
         (cellsize (app-cellsize app))
         (middle (* cellsize +middle-cell+))
         )
    (fill-rect renderer 0 0 boardsize boardsize)
    (sdl2:set-render-draw-color renderer 208 208 208 128) ; center cell skari
    (fill-rect renderer middle middle cellsize cellsize)
    (sdl2:set-render-draw-color renderer 32 32 32 240) ; board line skari
    (loop for x from 0 by cellsize repeat (1+ +board-cells+) do
          (line-hori renderer +board-line-width+ 0 x boardsize)
          (line-vert renderer +board-line-width+ x 0 boardsize))
    ; TODO draw the board pieces uh can we get a bitmap for those?
    ))

(defun update (app)
  (let ((renderer (app-rend app)))
    (sdl2:set-render-draw-color renderer 64 64 64 255) ; background skari
    (sdl2:render-clear renderer)
    (update-graph renderer app)
    (update-board renderer app)
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
  (multiple-value-bind (total size xoff yoff)
                       (square-size (- +game-width+ +border-x+)
                                    (- +game-height+ +border-y+)
                                    +board-cells+)
    (setf (app-cellwidth app) total
          (app-cellsize app) size
          (app-cellxoff app) xoff
          (app-cellyoff app) yoff))
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
