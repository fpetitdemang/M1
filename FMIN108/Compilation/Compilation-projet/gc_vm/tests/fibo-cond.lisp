(defun fibo-cond (n)
	(cond
		((= n 0) 1)
		((= n 1) 1)
		(t 
			(+ (fibo-cond (- n 1)) (fibo-cond (- n 2)))
		)
	)
)
(fibo-cond 5)