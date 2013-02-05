(defun addition-setf (x y)
	(let	(
			(result (+ x (* 2 y)))
		)
    		(setf result (* result 3))
    		result
	)
)
(addition-setf 15 5)