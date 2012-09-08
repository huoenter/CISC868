#!/usr/bin/sbcl --script

(defun rep-seq-helper (m n)
	(cond ((= 0 n) ())
		  (t (cons m (rep-seq-helper m (- n 1))))))

(defun rep-seq (lst)
	(cond ((null lst) ())
		  (t (cons (rep-seq-helper (car lst) (car lst)) (rep-seq (cdr lst))))))


(print (rep-seq '(3 5 3 1)))
