(defun fibo1 (n) (cond ((= n 0) 1) ((= n 1) 1) (t ( + (fibo1 (- n 1)) (fibo1 (- n 2))))))
 
(defun fibo2 (n) (if (<= n 1) 1 (+ (fibo2 (- n 1)) (fibo2 (- n 2)))))

(defun fibo3 (n r1 r2) (if (<= n 0) r2 (fibo3 (- n 1) (+ r1 r2) (- (+ r1 r2) r2))))

(defun fibo4 (n) (labels ((fibol4 (x y z) (if (<= x 0) z (fibol4 (- x 1) (+ y z) (- (+ y z) z))))) (fibol4 n 1 1)))

(defun fibo5 (n)
	(labels (
		(fibol1 (x) (if (<= x 1) 1 (+ (fibol2 (- x 1)) (fibol3 (- x 2)))))
		(fibol2 (x) (if (<= x 1) 1 (+ (fibol3 (- x 1)) (fibol1 (- x 2)))))
		(fibol3 (x) (if (<= x 1) 1 (+ (fibol1 (- x 1)) (fibol2 (- x 2))))))
	(fibol1 n)))