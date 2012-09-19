#!/usr/bin/sbcl --script 
(defun contains(n expr)
	(cond
	((null expr) nil)
	((and (atom expr) (equal expr n)) t)
	((equal (car expr) n) t)
	(t (contains n (cdr expr)))))

(defun depth (expr)
	(cond
	(t 10)))
	
(defun simplify1 (expr)
	(cond 
	((null expr) nil)
	((atom expr) expr)
	((equal (car (last expr)) '*) 
		(cond
		((contains 0 expr) 0)
		((equal (car expr) '1) (simplify1 (cdr expr)))
		(t (cons (simplify1 (car expr)) (simplify1 (cdr expr))))))
	((equal (car (last expr)) '+) 
		(cond
		((equal (car expr) '0) (simplify1 (cdr expr)))
		(t (cons (simplify1 (car expr)) (simplify1 (cdr expr))))))
	((equal (car (last expr)) '/)
		(cond
		((equal (car expr) 0) 0)
		((equal (cadr expr) 1) (simplify1 (car expr)))
		(t (list (simplify1 (car expr)) (simplify1 (cadr expr)) (car (last expr))))))
	((equal (car (last expr)) '%)
		(cond
		((equal (cadr expr) 1) (simplify1 (car expr)))
		((equal (cadr expr) 0) 1)
		(t (list (simplify1 (car expr)) (simplify1 (cadr expr)) (car (last expr))))))))
		


(defun simplify2 (expr)
	(cond
	((null expr) nil)
	((and (atom expr) (equal expr 0)) nil)
	((atom expr) expr)
	((null (simplify2 (car expr))) (simplify2 (cdr expr)))
	(t (cons (simplify2 (car expr)) (simplify2 (cdr expr))))))

(defun simplify3 (expr)
	(cond
	((atom expr) expr)
	((null (cddr expr)) (car expr))
	(t (mapcar #'(lambda (x) (simplify3 x)) expr))))

(defun simplify4 (expr n)
	(cond
	((= n 0) expr)
	(t (simplify4 (simplify1 expr) (- n 1)))))


(defun simplifye (expr)
	(simplify3 (simplify2 (simplify4 expr (depth expr)))))


(print (simplifye '(x (3 1 4 *) (5 1 *) 8 ((x y z *) 0 *) +)))
(print (simplifye '(x (3 1 4 *) (5 1 *) 8 (((x 1 /) y z *) (0 a /) *) +)))
(print (simplifye '((3 y *) (4 (x 5 %) *) +)))
(print (simplifye '((0 8 *) (x 0 %) +)))
(print (simplifye '((3 y *) ((4 (0 3 /) +) (x 1 %) *) +)))
