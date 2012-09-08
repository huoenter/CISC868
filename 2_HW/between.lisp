#!/usr/bin/sbcl --script
(defun count-between-helper (f s th)
	(> (* (- f s) (- s th)) 0))

(defun count-between (lst)
	(cond ((null (cddr lst)) ())
		  ((count-between-helper (car lst) (cadr lst) (caddr lst)) (cons (cadr lst) (count-between (cdr lst))))
		  (t (count-between (cdr lst)))))


(print (count-between '(3 2 4 5 7 8 6 4)))
(print (count-between '(83 40 25 54 77 66 83 102)))
