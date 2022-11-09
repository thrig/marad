; marad.lisp - the main game code
;
;   "... In ancient times it [Marad] was on the canal, Abgal, running
;   between Babylon and Isin. The city's main temple, a ziggurat, is
;   E-igi-kalama (House which is the eye of the land)"
;     -- https://en.wikipedia.org/wiki/Marad
;
; there may be things to TWEAK

(in-package :marad)

; TODO ask SDL what the screen size is and then derive a bunch of the
; following constants from that (and fail if the screen is too small?)
; TWEAK change this to make (probably) a bigger window. search also for
; fullscreen
(defconstant +game-width+ 1024)
(defconstant +game-height+ 768)

(defconstant +max-cell+ (1- +board-size+))
; this is the score cell, move into here to get points
(defconstant +middle-cell+ 4)
; don't let the board take up all the game window. NOTE the X border
; needs to have enough space to show a cell and a bit more
(defconstant +border-x+ 256)
(defconstant +border-y+ 40)
(defconstant +border-offx+ (/ +border-x+ 2))
(defconstant +border-offy+ (/ +border-y+ 2))
(defconstant +board-line-width+ 2)
(defconstant +board-extra-tweak+ 8)

(defconstant +src-indicator-size+ 16)

; the graph (background art) extends a bit beyond the window boundaries
(defconstant +graph-more+ 128)
(defconstant +graph-less+ (- (/ +graph-more+ 2)))
(defconstant +graph-line-width+ 4)
(defconstant +node-min-size+ 11)
(defconstant +node-max-size+ 83)

; optional adjustments for from where lines or such get drawn from (the
; background graph is drawn outside the window bounds, and the board
; from somewhere inside)
(defparameter *offset-x* 0)
(defparameter *offset-y* 0)

; probably lower this number on a slower system as a higher count can
; eat up CPU figuring out the graph for the background art. or just
; disable all that code.
(defconstant +node-count+ 128)

(defconstant +sdl-delay+ 128)   ; do not burn up the CPU too much

(defconstant +game-over+ -1)

; what cell in the board is selected
(defstruct (boardcell (:type vector))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

; bag of holding for "world" objects or whatever
(defstruct (app (:type vector))
  rend      ; SDL renderer
  graph     ; background graph art thing
  slope     ; a slope function for the graph
  numbers   ; textures for numbers because ENOSDLTTF
  pieces    ; textures for the game pieces
  board     ; game board
  state     ; funcall of how to handle a click
  src       ; boardcell of where a move is starting from
  (type +empty-cell+ :type fixnum) ; type of the src cell
  cellwidth ; how big the board is
  cellsize  ; how big a board cell is
  cellxoff  ; where to start drawing the board from
  cellyoff)

(defun draw-line (renderer x1 y1 x2 y2)
  (sdl2:render-draw-line renderer
                         (+ *offset-x* x1) (+ *offset-y* y1)
                         (+ *offset-x* x2) (+ *offset-y* y2)))

(defun draw-rect (renderer x1 y1 width height)
  (let ((rect (sdl2:make-rect (+ *offset-x* x1) (+ *offset-y* y1)
                              width height)))
    (sdl2:render-draw-rect renderer rect)
    (free-rect rect)))

(defun fill-rect (renderer x1 y1 width height)
  (let ((rect (sdl2:make-rect (+ *offset-x* x1) (+ *offset-y* y1)
                              width height)))
    (sdl2:render-fill-rect renderer rect)
    (free-rect rect)))

(defun render-rect (renderer texture x1 y1 width height)
  (let ((rect (sdl2:make-rect x1 y1 width height)))
    (sdl2:render-copy renderer texture :dest-rect rect)
    (free-rect rect)))

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
         (pieces (app-pieces app))
         (game (app-board app))
         (player (gameboard-player game)))
    ; draw the board and grid it up
    (fill-rect renderer 0 0 boardsize boardsize)
    (sdl2:set-render-draw-color renderer 208 208 208 128) ; center cell skari
    (fill-rect renderer middle middle cellsize cellsize)
    (sdl2:set-render-draw-color renderer 32 32 32 240) ; board line skari
    (loop for x from 0 by cellsize repeat (1+ +board-size+) do
          (line-hori renderer +board-line-width+ 0 x boardsize)
          (line-vert renderer +board-line-width+ x 0 boardsize))
    ; show the game pieces
    (with-board-pieces
      (game board row col owner type)
      (render-rect renderer (aref pieces owner (1- type))
                   (+ *offset-x* (* cellsize col))
                   (+ *offset-y* (* cellsize row))
                   cellsize cellsize))
    (let ((type (app-type app)))
      (cond ((plusp type)
              ; active cell indicator thing
              (sdl2:set-render-draw-color renderer 255 0 0 255) ; src skari
              (let ((src (app-src app)))
                (fill-rect renderer
                           (* cellsize (boardcell-x src))
                           (* cellsize (boardcell-y src))
                           +src-indicator-size+ +src-indicator-size+)))
            ((= type +game-over+)
              (sdl2:set-render-draw-color renderer 255 255 255 255)
              (fill-rect renderer 0 0 boardsize boardsize)
              (render-rect renderer (aref (app-numbers app) (1+ player))
                           (+ *offset-x* middle)
                           (+ *offset-y* middle)
                           cellsize cellsize))))
    ; display whose move is it with the move count
    (let ((move-count (gameboard-moves game))
          (numbers (app-numbers app))
          (score (gameboard-score game)))
      (sdl2:set-render-draw-color renderer 255 255 255 240) ; move bg skari
      (fill-rect renderer (- (* cellsize -1) +board-extra-tweak+) 0
                 cellsize cellsize)
      (fill-rect renderer (+ (* cellsize +board-size+) +board-extra-tweak+) 0
                 cellsize cellsize)
      (if (zerop player)
        (render-rect renderer (aref numbers move-count)
                     (- (+ *offset-x* (* cellsize -1))
                        +board-extra-tweak+)
                     *offset-y* cellsize cellsize)
        (render-rect renderer (aref numbers move-count)
                     (+ (+ *offset-x* (* cellsize +board-size+))
                        +board-extra-tweak+)
                     *offset-y* cellsize cellsize))
      (sdl2:set-render-draw-color renderer 255 255 255 240) ; score bg skari
      ; player 1 score
      (fill-rect renderer (- (* cellsize -1) +board-extra-tweak+)
                 (* cellsize +max-cell+) cellsize cellsize)
      (render-rect renderer (aref numbers (aref score 0))
                   (- (+ *offset-x* (* cellsize -1))
                      +board-extra-tweak+)
                   (+ *offset-y* (* cellsize +max-cell+))
                   cellsize cellsize)
      ; player 2 score
      (fill-rect renderer (+ (* cellsize +board-size+) +board-extra-tweak+)
                 (* cellsize +max-cell+) cellsize cellsize)
      (render-rect renderer (aref numbers (aref score 1))
                   (+ (+ *offset-x* (* cellsize +board-size+))
                      +board-extra-tweak+)
                   (+ *offset-y* (* cellsize +max-cell+))
                   cellsize cellsize))))

(defun update (app)
  (let ((renderer (app-rend app)))
    (sdl2:set-render-draw-color renderer 64 64 64 255) ; background skari
    (sdl2:render-clear renderer)
    (update-graph renderer app)
    (update-board renderer app)
    (sdl2:render-present renderer)))

; pick the piece to move, if it's a valid one
(defun movestate-set-active (app game row col)
  (when-let ((cell (get-cell game row col)))
    (let ((player (gameboard-player game)))
      (multiple-value-bind (type owner)
                           (cell-details cell)
        (when (= player owner)
          (let ((src (app-src app)))
            (psetf (app-type app) type
                   (app-state app) #'movestate-finalize
                   (boardcell-y src) row
                   (boardcell-x src) col)))))))

; move and advance the game if it's a valid move. if not a legal move
; the state remains in this state
(defun movestate-finalize (app game dsty dstx)
  (let* ((src (app-src app))
         (srcx (boardcell-x src))
         (srcy (boardcell-y src)))
    (multiple-value-bind (mtype stepx stepy)
                         (move-type srcx srcy dstx dsty)
      (when (legal-move? mtype (app-type app))
        (move-pushing (gameboard-board game) (gameboard-moves game)
                      srcx srcy stepx stepy)
        (if (next-turn game)
          (movestate-reset app)
          (psetf
            (app-type app) +game-over+
            (app-state app) #'movestate-gameover))))))

(defun movestate-gameover (app game &rest unused))

; reset the active (src) cell, probably from a click outside the board
(defun movestate-reset (app)
  (psetf (app-type app) +empty-cell+
         (app-state app) #'movestate-set-active))

; did they click somewhere within the game board? states here are to
; clear the source cell, set the source cell, and set the destination
; cell, which if legal should trigger a move
(defun click-to-board (app clickx clicky)
  (let* ((cellsize (app-cellsize app))
         (offx (app-cellxoff app))
         (offy (app-cellyoff app))
         ; TODO this should only be one value to subtract
         (x (floor (/ (- clickx offx +border-offx+) cellsize)))
         (y (floor (/ (- clicky offy +border-offy+) cellsize)))
         (game (app-board app)))
    (if (array-in-bounds-p (gameboard-board game) y x)
      (funcall (app-state app) app game y x)
      (movestate-reset app))))

(defun eventually (app)
  (sdl2:with-event-loop
    (:method :poll)
    (:keyup
      (:keysym keysym)
      (let ((scancode (sdl2:scancode-value keysym))
            (sym (sdl2:sym-value keysym))
            (mod-value (sdl2:mod-value keysym)))
        (cond
          ((sdl2:scancode= scancode :scancode-escape)
            (if (= (app-type app) +game-over+)
              (reset-world app)
              (movestate-reset app)))
          ((and (= mod-value 1) (sdl2:scancode= scancode :scancode-q)) ; Quit
            (sdl2:push-event :quit)))))
    (:mousebuttondown (:x x :y y :button button) (click-to-board app x y))
    (:idle () (update app) (sdl2:delay +sdl-delay+))
    (:quit () t)))

(defun load-image (renderer filename)
  (let ((surface (load-bmp filename)))
    (sdl2:set-color-key surface :true
                        (sdl2:map-rgb (sdl2:surface-format surface)
                                      255 255 255))
    (let ((texture (sdl2:create-texture-from-surface renderer surface)))
      (free-surface surface)
      texture)))

; TODO get SDL TTF working somehow, this is silly
; on the plus side this keeps the game from going on too long
(defun load-numbers (app renderer)
  (let ((numbers (make-array 16)) surface)
    (psetf (aref numbers 0) (load-image renderer "image/0.bmp")
           (aref numbers 1) (load-image renderer "image/1.bmp")
           (aref numbers 2) (load-image renderer "image/2.bmp")
           (aref numbers 3) (load-image renderer "image/3.bmp")
           (aref numbers 4) (load-image renderer "image/4.bmp")
           (aref numbers 5) (load-image renderer "image/5.bmp")
           (aref numbers 6) (load-image renderer "image/6.bmp")
           (aref numbers 7) (load-image renderer "image/7.bmp")
           (aref numbers 8) (load-image renderer "image/8.bmp")
           (aref numbers 9) (load-image renderer "image/9.bmp")
           (aref numbers 10) (load-image renderer "image/10.bmp")
           (aref numbers 11) (load-image renderer "image/11.bmp")
           (aref numbers 12) (load-image renderer "image/12.bmp")
           (aref numbers 13) (load-image renderer "image/13.bmp")
           (aref numbers 14) (load-image renderer "image/14.bmp")
           (aref numbers 15) (load-image renderer "image/15.bmp"))
    (setf (app-numbers app) numbers)))

; TODO how make these pieces load relative to the system or similar?
(defun load-pieces (app renderer)
  (let ((pieces (make-array '(2 3))) surface)
    (psetf (aref pieces 0 0) (load-image renderer "image/white.1.bmp")
           (aref pieces 0 1) (load-image renderer "image/white.2.bmp")
           (aref pieces 0 2) (load-image renderer "image/white.3.bmp")
           (aref pieces 1 0) (load-image renderer "image/black.1.bmp")
           (aref pieces 1 1) (load-image renderer "image/black.2.bmp")
           (aref pieces 1 2) (load-image renderer "image/black.3.bmp"))
    (setf (app-pieces app) pieces)))

(defun new-world ()
  (setq *random-state* (make-random-state t))
  (let ((app (make-app))
        (graph (new-graph (+ +game-width+ +graph-more+)
                          (+ +game-height+ +graph-more+) +node-count+)))
    (setf (app-board app) (new-gameboard)
          (app-state app) #'movestate-set-active
          (app-src app) (make-boardcell)
          (app-graph app) graph
          (app-slope app) (new-clamped-slope
                            (nodeset-pop-min graph) +node-min-size+
                            (nodeset-pop-max graph) +node-max-size+))
    (multiple-value-bind (total size xoff yoff)
                         (square-size (- +game-width+ +border-x+)
                                      (- +game-height+ +border-y+)
                                      +board-size+)
      (setf (app-cellwidth app) total
            (app-cellsize app) size
            (app-cellxoff app) xoff
            (app-cellyoff app) yoff))
    app))

(defun reset-world (app)
  (setq *random-state* (make-random-state t))
  (let ((graph (new-graph (+ +game-width+ +graph-more+)
                          (+ +game-height+ +graph-more+) +node-count+)))
    (psetf (app-board app) (new-gameboard)
           (app-state app) #'movestate-set-active
           (app-type app) +empty-cell+
           (app-graph app) graph
           (app-slope app) (new-clamped-slope
                             (nodeset-pop-min graph) +node-min-size+
                             (nodeset-pop-max graph) +node-max-size+))))

(defun main (&aux (app (new-world)))
  (sdl2:with-init
    (:everything)
    (sdl2:with-window
      (win :title "Marad" :w +game-width+ :h +game-height+ :flags '(:shown))
      ; TWEAK
      ;(sdl2:set-window-fullscreen win t)
      (sdl2:with-renderer
        (renderer win :flags '(:accelerated))
        (load-numbers app renderer)
        (load-pieces app renderer)
        (setf (app-rend app) renderer)
        (update app)
        (eventually app)))))

; NOTE some LISP implementations on some operating systems require that
; SLD2 be run from the main thread, hence make-this-thread-main. if that
; causes a problem you can probably call directly to #'main
(defun _start ()
  (sdl2:make-this-thread-main #'main)
  (fresh-line))
