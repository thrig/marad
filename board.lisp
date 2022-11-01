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

; TODO probably want sub-packages for /graph and /board
(in-package :marad)

; flags
(defconstant +board-player+ 6)
(defconstant +board-moved+  7)

; to textify the board with; player 0 gets PBK and player 1 lowercase
; thereof for Pawn Bishop King. the pieces however only somewhat move
; like chess pieces do
(defparameter *piece-char* '((0 . #\.) (1 . #\P) (2 . #\B) (3 . #\K)))

(defstruct (gameboard (:type vector))
  (size 0 :type fixnum)
  (turn 0 :type fixnum)
  (moves 0 :type fixnum) ; how many moves are available in the turnpair
  score
  board)

(defmacro board-set-type (type x) `(setf (ldb (byte 4 0) ,x) ,type))
(defmacro board-get-type (x) `(ldb (byte 4 0) ,x))

(defmacro board-get-flag (flag x) `(ldb (byte 1 ,flag) ,x))
(defmacro board-set-flag (flag x) `(setf (ldb (byte 1 ,flag) ,x) 1))

(defmacro with-gameboard ((game board row col) body)
  (let ((size (gensym)))
    `(let ((,size (gameboard-size game))
           (,board (gameboard-board game)))
       (dotimes (,row ,size)
         (dotimes (,col ,size)
           ,@body)))))

; call this at the beginning of each turn, and before saving
(defun clear-moved (game)
  (with-gameboard
    (game board row col)
    (setf (ldb (byte 1 +board-moved+)
               (aref board row col)) 0)))

; gameboard in the starting position. "moves" is yet to be determined,
; and it is the 0th turn. the pieces are vertical as I was thinking of
; Archon (1983) when playtesting the game with some coins on a
; cardboard grid
;
; NOTE "moves" of zero for a second player turn is an illegal state; the
; move count must be shared between pairs of turns
(defun new-gameboard (size)
  (let ((board (make-array `(,size ,size) :element-type '(unsigned-byte 8)
                           :initial-contents '((0 0 0 0 0 0 0 0 0)
                                               (0 2 0 0 0 0 0 66 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 3 0 0 0 0 0 67 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 1 0 0 0 0 0 65 0)
                                               (0 2 0 0 0 0 0 66 0)
                                               (0 0 0 0 0 0 0 0 0)))))
    (make-gameboard
      :size size :board board
      :score (make-array 2 :element-type 'fixnum :initial-element 0))))

; did something move in the given cell (probably the central scoring cell)
(defun moved? (game row col)
  (plusp (ldb (byte 1 +board-moved+)
              (aref (gameboard-board game) row col))))

(defun print-board (game &optional (stream t))
  (let ((score (gameboard-score game)))
    (format stream "Marad 1 turn ~d moves ~d score ~d ~d~&"
            (gameboard-turn game) (gameboard-moves game)
            (aref score 0) (aref score 1)))
  (let ((size (gameboard-size game))
        (board (gameboard-board game)))
    (dotimes (row size)
      (dotimes (col size)
        (let* ((cell (aref board row col))
               (type (board-get-type cell))
               (player (board-get-flag +board-player+ cell))
               (char (cdr (assoc type *piece-char*))))
          (write-char (if (plusp player)
                        (char-downcase char)
                        (char-upcase char))
                      stream)))
      (fresh-line))))
