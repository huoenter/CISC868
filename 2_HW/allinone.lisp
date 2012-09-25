;;;;
(defun between-helper (f s th)
	(> (* (- f s) (- s th)) 0))

(defun between (lst)
	(cond ((null (cddr lst)) ())
		  ((between-helper (car lst) (cadr lst) (caddr lst)) (cons (cadr lst) (between (cdr lst))))
		  (t (between (cdr lst)))))


;;;;
(defun count-between-helper (f s th)
	(> (* (- f s) (- s th)) 0))

(defun count-between (lst)
	(cond ((null (cddr lst)) 0)
		  ((count-between-helper (car lst) (cadr lst) (caddr lst)) (+ 1 (count-between (cdr lst))))
		  (t (count-between (cdr lst)))))


;;;;


(defun emit (aa)
	(cond ((equal aa 'X) 1)
		  (t 0)))

(defun derivative-helper (expr)
	(cond ((atom expr) (emit expr))
		  ((equal (last expr) '(+)) (list (derivative-helper (car expr)) (derivative-helper (cadr expr)) '+))
		  ((equal (last expr) '(*)) 
		  	(list (list (car expr) (derivative-helper (cadr expr)) '*) (list (cadr expr) (derivative-helper (car expr)) '*) '+))
		  ((equal (last expr) '(%))
		  	(list (cadr expr) (list (car expr) (- (cadr expr) 1) '%) '*))
		  (t '(err))))

(defun derivative (expr)
	(derivative-helper expr))

;;;;

(defun full-inherit-get-helper (objects property)
	(let ((ans (get (car objects) property)))
	(cond
	((null objects) nil)
	((equal ans nil) (full-inherit-get-helper (append (cdr objects) (get (car objects) 'isa)) property))
	(t ans))))

	

(defun full-inherit-get (object property)
	(full-inherit-get-helper (list object) property ))
;	(t (mapcar #'(lambda (x) (full-inherit-get x property)) (get object 'isa))))))



;;;;

(defun infix (expr)
	(cond ((atom expr) expr) 
		  (t (list (infix (cadr expr)) (car expr) (infix (caddr expr))))))

;;;;

(defun inherit-get (object property)
	(let ((p (get object property)))
	(cond
	((equal object nil) nil)
	((equal p nil) (inherit-get (get object 'isa) property))
	(t p))))

;;;;

(defun meld-helper (struct aa left right)
;	(format t "~A   ~A~%" left right)
	(cond ((null right) left)
		  ((equal aa (car right)) (append left struct (meld-helper struct aa (list aa) (cdr right ))))
		  ((atom (car right)) (meld-helper struct aa (append left (list (car right))) (cdr right))) 
		  (t (append left (list (meld-helper struct aa () (car right))) (meld-helper struct aa () (cdr right))))))

(defun meld (struct aa lst)
	(meld-helper struct aa () lst))

;;;;

(defun rep-seq-helper (m n)
	(cond ((= 0 n) ())
		  (t (cons m (rep-seq-helper m (- n 1))))))

(defun rep-seq (lst)
	(cond ((null lst) ())
		  (t (cons (rep-seq-helper (car lst) (car lst)) (rep-seq (cdr lst))))))


;;;;
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
		((equal (simplify1 (car expr)) '1) (simplify1 (cdr expr)))
		(t (cons (simplify1 (car expr)) (simplify1 (cdr expr))))))
	((equal (car (last expr)) '+) 
		(cond
		((equal (simplify1 (car expr)) '0) (simplify1 (cdr expr)))
		(t (cons (simplify1 (car expr)) (simplify1 (cdr expr))))))
	((equal (car (last expr)) '/)
		(cond
		((equal (simplify1 (car expr)) 0) 0)
		((equal (simplify1 (cadr expr)) 1) (simplify1 (car expr)))
		(t (list (simplify1 (car expr)) (simplify1 (cadr expr)) (car (last expr))))))
	((equal (car (last expr)) '%)
		(cond
		((equal (simplify1 (cadr expr)) 1) (simplify1 (car expr)))
		((equal (simplify1 (cadr expr)) 0) 1)
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
	((null (cdr expr)) nil)
	(t (mapcar #'(lambda (x) (simplify3 x)) expr))))

(defun simplify4 (expr n)
	(cond
	((= n 0) expr)
	(t (simplify4 (simplify1 expr) (- n 1)))))

(defun simplify5 (expr)
	(cond
	((null expr) nil)
	((atom expr) expr)
	(t (list (simplify4 (car expr)) (simplify4 (cdr expr))))))


(defun simplifye (expr)
	(simplify3 (simplify2 (simplify4 expr (depth expr)))))
	;(simplify3 (simplify2 (simplify4 expr))))
	;(simplify1 expr))


;;;;

(defun visit (int1 root)
	(cond ((>= root int1) (list root))
		  (t ())))

(defun subtree-geq (int1 tree)
	(cond ((null tree) ())
		  (t (append (subtree-geq int1 (cadr tree)) (visit int1 (car tree)) (subtree-geq int1 (caddr tree))))))
