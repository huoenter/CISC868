#!/usr/bin/sbcl --script

(defun infix (expr)
	(cond ((atom expr) expr) 
		  (t (list (infix (cadr expr)) (car expr) (infix (caddr expr))))))

(print (infix '(+ 2 3)))
(print (infix '(* (+ 1 2) (- 10 8))))
(print (infix '(* (+ (- 3 4) (/ 4 6)) 5)))
