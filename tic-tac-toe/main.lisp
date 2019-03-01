
#|
generate moves (state transitions)

generate states to search

categorize states

generalize to wuziqi
|#


(defun play ()
  (let ((board (make-array '(3 3) :initial-element '_))
        (turn 'X)
        (row 0)
        (col 0))
    (print-arr board)
    (loop while (not (terminalp board row col))
          do (progn (setf row (read))
                    (setf col (read))
                    (when (move row col board turn)
                      (setf turn (flip turn)))
                    (print-arr board)))
    (format t "~A wins!" (aref board row col))))

(defun flip (elem)
  (cadr (assoc elem '((X O) (O X)))))

(defun in-bounds (row col)
  (and (>= row 0)
       (>= col 0)
       (< row 3)
       (< col 3)))

(defun move (row col board turn)
  ;; TODO ... check for invalid input, printout doesn't show
  (let ((color (aref board row col)))
    (if (and (in-bounds row col)
             (equal color '_))
        (progn (setf (aref board row col) turn) t)
        (format t "invalid move, retry~%"))))

(defun print-arr (arr)
  (dotimes (row 3)
    (dotimes (col 3)
      (format t "~A " (aref arr row col)))
    (format t "~%")))

;; so i guess assignment is like iterating over a list...
;; and then binding variables to pass to a function called once for ea var in list
;; whose result is collected over and over
;; lets try haskell'ing this ...
;; how to apply the closures? ch 6 on lisp ..
(defun terminalp (board row col)
  (let ((color (aref board row col)))
    (when (equal color '_)
      (return-from terminalp nil))
    (loop
      for (ur uc) in '((0 1) (1 1) (1 0) (1 -1))
      for in-a-row = (+ 1 (loop for coeff in '(1 -1)
                                for dr = (* coeff ur)
                                for dc = (* coeff uc)
                                sum (loop for nr = (+ row dr) then (+ nr dr)
                                          for nc = (+ col dc) then (+ nc dc)
                                          while (and (in-bounds nr nc)
                                                     (equal color (aref board nr nc)))
                                          count nr)))
      while (< in-a-row 3) finally (return (= in-a-row 3)))))

;;; add more test cases to this ... add func for showing result on each one
(defun test-terminalp ()
  (let ((board (make-array '(3 3) :initial-element '_)))
    (labels ((set-cell (row col val)
               (setf (aref board row col) val))
             (set-coords (coords val)
               (loop for (row col) in coords
                     do (set-cell row col val))))
      (set-coords '((0 0) (0 1) (0 2)) 'X))
    (print-arr board)
    (format t "~A~%"
            (if (terminalp board 0 2)
                "game over"
                "continue"))))
