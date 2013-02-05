 
(defun calc(l)
	(if (atom l) l
	
	(apply (car l) (list (calc (cadr l)) (calc (caddr l))))))

(defun calc1(l)
	(if (<= (length l) 3) (calc l)
	(calc1 (cons (car l) (cons (calc (list (car l) (cadr l) (caddr l))) (cdddr l))))))

(defun calc2(l)
	(cond ((atom l) l)
	((eql 'quote (car l)) (cadr l))
	(apply (car l) (list (calc2 (cadr l)) (calc2 (caddr l))))))
