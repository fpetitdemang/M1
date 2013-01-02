(defun fibo-cond (n)
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(t 
			(+ (fibo-cond (- n 1)) (fibo-cond (- n 2)))
		)
	)
)
(fibo-cond 7)