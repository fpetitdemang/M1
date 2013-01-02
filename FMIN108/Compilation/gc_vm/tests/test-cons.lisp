(defun test-cons (l)
	(cond
		((eql (cdr l) NIL) (cons (car l) NIL))
		(t 
			(cons (car l) (test-cons (cdr l)))
		)
	)
)
(test-cons '(1 2 3 4))