(defun example-cond (l)
	(cond
		((eql (cdr l) NIL) (cons (car l) NIL))
		(t 
			(cons (car l) (example-cond (cdr l)))
		)
	)
)
(example-cond '(1 2 3 4))