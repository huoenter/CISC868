#!/usr/bin/sbcl --script

(defun inherit-get (object property)
	(let ((p (get object property)))
	(cond
	((equal object nil) nil)
	((equal p nil) (inherit-get (get object 'isa) property))
	(t p))))

(setf (get 'bird 'wings) 2)
(setf (get 'canary 'isa) 'bird)
(print (inherit-get 'canary 'wings))
(print (inherit-get 'canary 'arms))
