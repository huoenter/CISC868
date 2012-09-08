#!/usr/bin/sbcl --script

(defun meld-helper (struct aa left right)
;	(format t "~A   ~A~%" left right)
	(cond ((null right) left)
		  ((equal aa (car right)) (append left struct (meld-helper struct aa (list aa) (cdr right ))))
		  ((atom (car right)) (meld-helper struct aa (append left (list (car right))) (cdr right))) 
		  (t (append left (list (meld-helper struct aa () (car right))) (meld-helper struct aa () (cdr right))))))

(defun meld (struct aa lst)
	(meld-helper struct aa () lst))

(print (meld '(6 (9)) 'b '(5 b 7)))
(print (meld '((7 8)) 'c '(c g f (c))))
(print (meld '(4 a (c)) 3 '((a b) 2 (4 6 (3)) (7) 3 (3 8))))
