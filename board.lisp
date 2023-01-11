; board.lisp - the game board
;
;   "These include the first recorded example of trash talk, between two
;   Senet players who appear painted on the wall of the tomb of the
;   provincial governor Pepiankh in Meir. The first player taunts his
;   opponent: It has alighted. By happy my heart, for I shall cause you
;   to see it taken away ... The second player then replies, You speak
;   as one weak of tongue, for passing is mine."
;     -- Board Games in 100 Moves. Ian Livingston and James Wallis. p.22-23
;
; The board is an array of uint8_t; bits 0..3 are for the piece types
; though not all of those bits are used; 4, 5 are free and 6,7 are for
; player ownership and did this piece move?

(in-package :marad)

(defconstant +board-size+ 9)

; TWEAK how many points it takes to win. KLUGE must be kept below 16 for
; reasons you might discover elsewhere (hint: load-numbers)
(defconstant +victory-points+ 15)
(defconstant +no-moves+ 0)

(defconstant +empty-cell+ 0)
(defconstant +move-other+ 0)
(defconstant +move-square+ 1)   ; also, the Rook type
(defconstant +move-diagonal+ 2) ; also, the Bishop type
; King is "3", which is both a Rook and a Bishop. see how that works?

; flags
(defconstant +board-player+ 6)
(defconstant +board-moved+  7)

; to textify the board with; player 0 gets RBK and player 1 lowercase
; thereof for Rook Bishop King. the pieces however only somewhat move
; like chess pieces do
(defparameter *piece-char* '((0 . #\.) (1 . #\R) (2 . #\B) (3 . #\K)))

; "0" for a move indicates that the entropy for the turnpair has not yet
; been determined, or is an error if player 2 is to move
(deftype move-count () '(integer 0 4))

(defstruct (gameboard (:type vector))
  (moves 0 :type move-count)
  (player 0 :type (unsigned-byte 1))
  (score nil :type (simple-array fixnum (2)))
  (victor 0 :type (unsigned-byte 1))
  board)

(defmacro board-set-type (type x) `(setf (ldb (byte 4 0) ,x) ,type))
(defmacro board-get-type (x) `(ldb (byte 4 0) ,x))

(defmacro board-get-flag (flag x) `(ldb (byte 1 ,flag) ,x))
(defmacro board-set-flag (flag x) `(setf (ldb (byte 1 ,flag) ,x) 1))

(defmacro toggle (x) `(setf ,x (logxor ,x 1)))

(defmacro with-gameboard ((game board row col) &body body)
  `(let ((,board (gameboard-board game)))
     (dotimes (,row +board-size+)
       (dotimes (,col +board-size+)
         ,@body))))

(defmacro with-board-pieces ((game board row col owner type) &body body)
  (let ((cell (gensym)))
    `(with-gameboard
       (,game ,board ,row ,col)
       (let ((,cell (aref ,board ,row ,col)))
         (when (plusp ,cell)
           (let* ((,type (board-get-type ,cell))
                  (,owner (board-get-flag +board-player+ ,cell)))
             ,@body))))))

(defun cell-details (cell)
  (values (board-get-type cell)
          (board-get-flag +board-player+ cell)))

(defun clear-moved (game)
  (with-gameboard
    (game board row col)
    (setf (ldb (byte 1 +board-moved+)
               (aref board row col)) 0)))

(defun get-cell (game row col)
  (let ((cell (aref (gameboard-board game) row col)))
    (when (plusp cell) cell)))

(defun legal-move? (typea typeb)
  (plusp (logand typea typeb)))

; this is rolled at the beginning of each turnpair
(defun move-count () (1+ (random 4)))

(defun move-pushing (board moves srcx srcy stepx stepy)
  (labels ((swap (a b x z)
             (board-set-flag +board-moved+ (aref board b a))
             (rotatef (aref board b a) (aref board z x)))
           (stepwise (a b)
             (if (array-in-bounds-p board b a)
               (if (plusp (aref board b a))
                 (let* ((newa (+ a stepx)) (newb (+ b stepy))
                        (result (stepwise newa newb)))
                   (ecase result
                     (:stop :stop)
                     (:move (swap a b newa newb) :move)))
                 :move)
               :stop)))
    (loop with nextx = (+ srcx stepx) and nexty = (+ srcy stepy)
          repeat moves do
          (let ((result (stepwise nextx nexty)))
            (ecase result
              (:stop (return))
              (:move (swap srcx srcy nextx nexty)
                     (psetf srcx nextx srcy nexty)
                     (incf nextx stepx)
                     (incf nexty stepy)))))))

; a gameboard in the starting position. the pieces are vertical as I was
; thinking of Archon (1983) when playtesting the game with some coins on
; a piece of ruled cardboard
(defun new-gameboard ()
  (let ((board (make-array `(,+board-size+ ,+board-size+)
                           :element-type '(unsigned-byte 8)
                           :initial-contents '((0 0 0 0 0 0 0 0 0)
                                               (0 2 0 0 0 0 0 66 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 3 0 0 0 0 0 67 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 2 0 0 0 0 0 66 0)
                                               (0 0 0 0 0 0 0 0 0)))))
    (make-gameboard :board board
                    :moves (move-count)
                    :score (make-array 2 :element-type 'fixnum
                                       :initial-element 0))))

(defun next-turn (game)
  ; increase score for the player whose moved piece is in the middle
  (let ((cell (aref (gameboard-board game) +middle-cell+ +middle-cell+)))
    (when (plusp (ldb (byte 1 +board-moved+) cell))
      (let ((player (board-get-flag +board-player+ cell)))
        (when (>= (incf (aref (gameboard-score game) player)) +victory-points+)
          (psetf (gameboard-moves game) +no-moves+
                 (gameboard-victor game) player)
          (return-from next-turn)))))
  ; in theory we need only clear the moved bit from the scoring cell,
  ; and only the whole board only before saving the game, but this keeps
  ; the board clean
  (clear-moved game)
  (let ((player (toggle (gameboard-player game))))
    (when (zerop player)
      (setf (gameboard-moves game) (move-count))))
  t)

; is a move square, diagonal, or not good? PORTABILITY SBCL is okay with
; not returning enough VALUES for the "not good" condition, maybe
; another LISP might complain about that, in which case add some nil to
; pad to three the return VALUES
(defun move-type (x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2))
    (values +move-other+)
    (let ((dy (- y2 y1)))
      (if (zerop dy)
        (values +move-square+ (if (> x2 x1) 1 -1) 0)
        (let ((dx (- x2 x1)))
          (if (zerop dx)
            (values +move-square+ 0 (if (> y2 y1) 1 -1))
            (if (= 1 (abs (/ dy dx)))
              (values +move-diagonal+
                      (if (plusp dx) 1 -1)
                      (if (plusp dy) 1 -1))
              (values +move-other+))))))))

(defun print-board (game &optional (stream t))
  (let ((score (gameboard-score game)))
    (format stream "Marad 1 player ~d moves ~d score ~d ~d~&"
            (gameboard-player game) (gameboard-moves game)
            (aref score 0) (aref score 1)))
  (let ((board (gameboard-board game)))
    (dotimes (row +board-size+)
      (dotimes (col +board-size+)
        (let* ((cell (aref board row col))
               (type (board-get-type cell))
               (player (board-get-flag +board-player+ cell))
               (char (cdr (assoc type *piece-char*))))
          (write-char (if (plusp player)
                        (char-downcase char)
                        (char-upcase char))
                      stream)))
      (fresh-line))))
