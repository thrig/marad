; graph.lisp - spatial network graph support
;
;   "The most important elements of this new ability are that (i) humans
;   have been able to create formal roles which are independent of an
;   actual individual, and (ii) among these formal roles, any kind of
;   network of relations -- that is, e.g., hierarchy -- is conceivable"
;     -- Why we live in hierarchies: a quantitative treatise. Anna
;        Zafeiris and Tamás Vicsek. 2017.
;
; "Emergence of hierarchy in cost-driven growth of spatial networks"
; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3670315/
; https://github.com/thrig/ministry-of-silly-vaults/tree/master/spatial-network
;
; The following code, however, has probably no impact on gameplay. It is
; used to draw the background art, as I could not think of any way to
; use the graph in a game.

(in-package :marad)

; a node in the network
;
; NOTE the custom print function avoids infinite loops when printing
; (changing *print-circle* does not help)
(defstruct (snode (:print-function print-snode))
  (xx 0 :type fixnum)
  (yy 0 :type fixnum)
  (population 0 :type double-float)
  (next nil :type (or null snode))
  (peers nil :type list))

; a set of nodes
(defstruct (nodeset (:type vector))
  (pop-min most-positive-long-float :type double-float)
  (pop-max 0.0d0 :type double-float)
  (network nil :type list)
  (unconn nil :type list))

; PORTABILITY works in SBCL but might not be portable to other LISP that
; are problematic with operations at or maybe past the end of the list
(defmacro list-splice (n list)
  `(if (zerop ,n)
     (prog1 (car ,list)
            (setf ,list (cdr ,list)))
     (let ((prev (nthcdr (1- ,n) ,list)))
       (prog1 (cadr prev)
              (rplacd prev (cddr prev))))))

; TODO poisson disk sample instead to keep the points more apart?
(defmacro with-random-points ((rows cols n row col) &body body)
  (let ((total (gensym)) (remain (gensym)) (maxcol (gensym)))
    `(do* ((,total (* ,rows ,cols) (1- ,total))
           (,remain (min ,n ,total))
           (,row 0) (,col 0) (,maxcol (1- ,cols)))
          ((<= ,remain 0))
       (when (< (random 1.0) (/ ,remain ,total))
         (progn ,@body)
         (decf ,remain))
       (if (>= ,col ,maxcol)
         (setf ,col 0 ,row (1+ ,row))
         (incf ,col)))))

; NOTE take sqrt to get actual distance
(defun distance (x1 y1 x2 y2)
  (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))

; pick the best node and wire it into the network
(defun highlander (ndset &aux (best most-negative-double-float) index link-to)
  (loop for i from 0 below (list-length (nodeset-unconn ndset)) do
        (loop for nn in (nodeset-network ndset) do
              (let ((skr (score (nth i (nodeset-unconn ndset)) nn)))
                (when (> skr best)
                  (setf best skr index i link-to nn)))))
  (let ((node1 (list-splice index (nodeset-unconn ndset))))
    (push link-to (snode-peers node1))
    (push node1 (snode-peers link-to))
    (setf (snode-next node1) link-to)
    (push node1 (nodeset-network ndset))))

; linear limit of inputs along the axis x1..x2 to the range y1..y2
(defun new-clamped-slope (x1 y1 x2 y2)
  (let* ((m (/ (- y2 y1) (- x2 x1))) (b (- y1 (* m x1))))
    (lambda (x)
      (let ((y (+ (* m x) b)))
        (if (> y y2) y2 (if (< y y1) y1 y))))))

(defun new-graph (width height node-count &aux (nset (make-nodeset)))
  (with-random-points
    (width height node-count x y)
    (let ((pop (powerlaw)))
      (when (< pop (nodeset-pop-min nset))
        (setf (nodeset-pop-min nset) pop))
      (when (> pop (nodeset-pop-max nset))
        (setf (nodeset-pop-max nset) pop))
      (push (make-snode :yy y :xx x :population pop)
            (nodeset-unconn nset))))
  ; TODO this instead should be a random point
  (push (pop (nodeset-unconn nset)) (nodeset-network nset))
  (loop while (nodeset-unconn nset) do (highlander nset))
  nset)

(defun print-snode (n stream depth)
  (declare (ignore depth))
  (format stream "#<SNODE [~d,~d] ~$>"
          (snode-yy n) (snode-xx n) (snode-population n)))

(defun powerlaw ()
  (expt (- 1.0d0 (random 1.0d0)) -0.47619047619047616d0)) ; (-1 / $alpha)

(defun score (node1 node2 &aux (power 1.1))
  (let ((dist (distance (snode-xx node1) (snode-yy node1)
                        (snode-xx node2) (snode-yy node2))))
    (- (/ (* (snode-population node1) (snode-population node2))
          (expt dist (- power 1)))
       (* 0.0001d0 dist)))) ; $BBB
