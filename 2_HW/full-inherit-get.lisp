#!/usr/bin/sbcl --script

(defun full-inherit-get-helper (objects property)
	(let ((ans (get (car objects) property)))
	(cond
	((equal ans nil) (full-inherit-get-helper (append (cdr objects) (get (car objects) 'isa)) property))
	(t ans))))

	

(defun full-inherit-get (object property)
	(full-inherit-get-helper (list object) property ))
;	(t (mapcar #'(lambda (x) (full-inherit-get x property)) (get object 'isa))))))


(setf (get 'baking-soda 'isa) '(rising-agent deodorant antacid))
(setf (get 'rising-agent 'isa) '(water))
(setf (get 'water 'edible) 'water)
(setf (get 'antacid 'edible) 'antacid)

(print (full-inherit-get 'baking-soda 'edible))
