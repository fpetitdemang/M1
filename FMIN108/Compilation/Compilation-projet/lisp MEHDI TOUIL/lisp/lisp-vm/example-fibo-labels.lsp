(defun fibo-labels (x)
	(labels (
			(f1 (n) 
				(if (= n 0) 
					0
					
					(if (= n 1)
						1
						(+ (f2 (- n 1)) (f2 (- n 2)))
					)
				)
			)
			(f2 (n) 
				(if (= n 0)
					0
					(if (= n 1)
						1
						(+ (f1 (- n 1)) (f1 (- n 2)))
					)
				)
			)
		)
		(f1 x)
	)
)

(fibo-labels 7)
