(defun fibo (x)
	(let 	(
			(a 2) (b 1)
		)
		(labels	(
				(f1 (x z) 
					(labels (
							(f3 (y) (if (< y a) 
									b
									(progn 
										(setf y (- y b))
										(+ (f2 ((lambda (y) (- y z)) y)) (f2 y))
									)
								)
							)
						) 
						(f3 (+ x a -2))
					)
				)
				(f2 (x) 
					(if (< x a) 
						b 
						(let 	(
								(x (- x 1))
							)
							(+ (f1 (- x 1) (- a 1)) (f1 x b))
						)
					)
				)
			)
		(f1 x 1))
	)
)
(fibo 10)