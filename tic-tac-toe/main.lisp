
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

;;; use more loop syntax .. make the loops return values ...
(defun terminalp (board row col)
  (let ((color (aref board row col)))
    (when (equal color '_)
      (return-from terminalp nil))
    (loop for (ur uc) in '((0 1) (1 1) (1 0) (1 -1))
          do (let ((length 1))
               (loop for coeff in '(1 -1)
                     do (let ((dr (* coeff ur))
                              (dc (* coeff uc)))
                          (do ((nr (+ row dr) (+ nr dr))
                               (nc (+ col dc) (+ nc dc)))
                              ((not (and (in-bounds nr nc)
                                         (equal color (aref board nr nc)))))
                            (incf length)))
                        (when (= length 3)
                          (return-from terminalp t))))))
  nil)

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
