;;;; advent of code funcs, playing with lisp

;;; rerun signature
(format t "~%hello~%")

;; call fn on each number given by stdin
(defun get-all-nums (fn lst)
  (let ((x (read nil nil)))
    ; terminate if eof or fn returns true
    (if (or (null x) (funcall fn x lst))
        ()
        (get-all-nums fn lst))))

(defun nullto0 (x)
  (if (null x) 0 x))

;; updates *prefixSums* with the most recent number
(defun matchPrevSum (x prefixsums)
  (let ((freq (+ x (nullto0 (car prefixsums)))))
    ; return true if latest prefix sum was found
    (if (member freq prefixsums)
        t
        (progn
          (format t "cur freq is ~A~%" freq)
          (setf prefixsums (cons freq prefixsums))
          nil))))

(defun print-list (description lst)
  (progn
    (format t description)
    (mapcar #'(lambda (x) (format t "~A " x)) lst)))

(defun find-match-sum (lst)
   (progn
    (get-all-nums 'matchPrevSum lst)
    (print-list "printing all prefix sums~%" lst)
    (format t "the first sum to match a prev sum is ~A~%" (car lst))))

(find-match-sum nil)
