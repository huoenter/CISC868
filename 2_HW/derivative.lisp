#!/usr/bin/sbcl --script

(defun emit (aa)
	(cond ((equal aa 'X) 1)
		  (t 0)))

(defun derivative (expr)
	(cond ((atom expr) (emit expr))
		  ((equal (last expr) '(+)) (list (derivative (car expr)) (derivative (cadr expr)) '+))
		  ((equal (last expr) '(*)) 
		  	(list (list (car expr) (derivative (cadr expr)) '*) (list (cadr expr) (derivative (car expr)) '*) '+))
		  ((equal (last expr) '(%))
		  	(list (cadr expr) (list (car expr) (- (cadr expr) 1) '%) '*))
		  (t '(err))))

(print (derivative '(3 (X 1 %) *)))
(print (derivative '((3 X *) (X 4 %) +)))
