(defun meval (expr) (eval expr))
(defun meval-body (le env)
	(if (null (cdr le)) (meval (car le)) (progn (meval (car le) ) (meval-body (cdr le) env))))

(defun meval-args (le env)
	(if (atom le) '() (cons (meval (car le)) (meval-args (cdr le) env))))

(defun make-env (ls lv env)
	(cond ((and (atom ls) (atom lv)) 
 		env)
 		((atom ls) (warn "liste des symboles trop petite") env)
		((atom lv) (warn "liste des valeurs trop petite") env)
 	      (t   (make-env (cdr ls) (cdr lv) (cons (cons (car ls) (car lv)) env)))))

(defun make-env-gen (ls lv env)
	(cond ((and (atom ls) (atom lv)) 
 		env)
 		((atom ls) (warn "liste des symboles trop courte") env)
		((atom lv) (warn "liste des valeurs trop courte") env)
 	      	((eql (car ls) '&optional (make-env-opt (cdr ls)  lv env)))
 	      	 (t   (make-env (cdr ls) (cdr lv) (cons (cons (car ls) (car lv)) env)))
))
(defun make-env-opt(ls lv env)
 		(cond ((atom (cdr ls)) (
 
 


(defun make-env-rest(ls lv env)
		(if (atom ls) (warn "symbole manquant")
		(cons (cons (car ls) lv) env)))

(defun foo(x) (let ((x (* x x))) (* 2 x)))