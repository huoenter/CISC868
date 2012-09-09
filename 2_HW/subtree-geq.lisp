#!/usr/bin/sbcl --script

(defun visit (int1 root)
	(cond ((>= root int1) (list root))
		  (t ())))

(defun subtree-geq (int1 tree)
	(cond ((null tree) ())
		  (t (append (subtree-geq int1 (cadr tree)) (visit int1 (car tree)) (subtree-geq int1 (caddr tree))))))


(print (subtree-geq 12 '(35 (6 (3 () ()) (17 () (19 () ()))) (48 (40 () ()) (74 (60 () ()) ()) ))))
(print (subtree-geq 50 '(35 (6 (3 () ()) (17 () (19 () ()))) (48 (40 () ()) (74 (60 () ()) ()) ))))
(print (subtree-geq 3 '(35 (6 (3 () ()) (17 () (19 () ()))) (48 (40 () ()) (74 (60 () ()) ()) ))))
(print (subtree-geq 19 '(35 (6 (3 () ()) (17 () (19 () ()))) (48 (40 () ()) (74 (60 () ()) ()) ))))
