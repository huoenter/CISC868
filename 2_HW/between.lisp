#!/usr/bin/sbcl --script
(defun between-helper (f s th)
	(> (* (- f s) (- s th)) 0))

(defun between (lst)
	(cond ((null (cddr lst)) ())
		  ((between-helper (car lst) (cadr lst) (caddr lst)) (cons (cadr lst) (between (cdr lst))))
		  (t (between (cdr lst)))))


(print (between '(3 2 4 5 7 8 6 4)))
(print (between '(83 40 25 54 77 66 83 102)))
