
;;; i could make something a macro ... for scope ..?

(defun play ()
  (let ((board (make-array '(3 3) :initial-element '_))
	(turn 'X))
    (print-arr board)
    (do* ((row (read) (read)) (col (read) (read)))
	 ((< row 0)
	  (< col 0))
      (when (move row col board turn)
	(setf turn (flip turn)))
      (print-arr board))))

(defun flip (elem)
  (cadr (assoc elem '((X O) (O X) (_ _)))))

(defun move (row col board turn)
  (let ((color (aref board row col)))
    (if (equal color '_)
	(progn (setf (aref board row col) turn) t)
	(format t "invalid move, retry~%"))))

(defun print-arr (arr)
  (let ((last-dim (length (array-dimensions arr))))
    (labels ((arr-helper (coords cur-dim)
	       (if (equal cur-dim last-dim)
		   (format t "~A " (apply #'aref (cons arr (reverse coords))))
		   (progn
		     (dotimes (dim-val (array-dimension arr cur-dim))
		       (arr-helper (cons dim-val coords) (+ 1 cur-dim)))
		     (dotimes (x (- last-dim cur-dim))
		       (format t "~%"))))))
      (arr-helper nil 0))))














;;; print the board
;; 1-d prints a line of values
;; 2-d prints a square of values
;; 3-d prints a line of squares of values
;; 4-d prints a square of squares of values
;; 5-d prints a line of squares of squares
;; ... and so on ...
;; the spacing between diff dimensions for repeated shapes will increase by a noticeable amount
;; noticeable might mean more than linear increase ofc

;;; simple version
#|(defun print-array (arr)
  (dotimes (row 3)
    (dotimes (col 3)
      (format t "~A " (aref arr row col)))
    (format t "~%")))
|#

;;; update game state
;;; linked list is not good for this, use an array

;;; todo maintain a count for each row and col for efficiency
;;; check each direction including row/col
;;; generalize these things eh?
;;; direction depends on # of dimensions
;;; basically you try to draw a line
;;; lin alg related concepts eh?

;;; generalized in-a-row: a line
    ;;; ... of any slope
	;;; just means that all points have the same ratio when differences are taken between them
	;;; an x-in-a-row means that the distance between x points is same and slopes are same...
    ;;; of only horiz, vert, 2 diags
        ;;; d dimensions, 2 directions per dimension
	;;; 1, 0, -1 -- ternay combos for 2 dimensions
#|

enumerate all configs of {-1,0,1}^d where d is number of dimensions
then remove "duplicate" line representations

these are slope values where going exact opposite is the same thing

|#

#|
(defun game-over (row col)
  (let ((dirs '((-1 0) (-1 1) (0 1) (1 1)))
	(coeffs (1 -1))
	(r row)
	(c col))
    (dolist (dir dirs)
      (let ((x car dir) (y cadr dir) (sum 0))
	(dolist (coeff coeffs)
	  (let ((coord '((+ x 
|#
;; move in a single direction


;;; (game-over row col)))))

;; state
;; transition table/rules
;; state exploration, categorization, organization

#|
(defun run (state)
  (if (terminalp state)
      (state)
      (run (next state))))

(defun run (state)
  `(print-state state)
  (if (terminalp state)
      (state)
      (run (next state))))

(defun start ()
  (step (init-state)))

;;;; could we use macros to generate the simple 3 in a row func
;;;; and then call that? ... do computation during compile time for
;;;; creating that ..

|#
