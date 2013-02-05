(defun automate(etats fin init transitions)
	(()))
(defun exp (mot)
	(labels ((et0 (m) 
		(cond ((null m) '())
		      ((eql (car m) 'a) (et1 (cdr m)))
		      ((eql (car m) 'b) (et0 (cdr m)))
		      ;((not (or (eql (car mot) a) (eql (car mot) b))) f)))
		      ))			 
		(et1 (m) 
		(cond ((null m) t)
		      ((eql (car m) 'a) (et1 (cdr m)))
		      ((eql (car m) 'b) (et0 ( cdr m))))))
(et0 mot)))
		      ;((not (or (eql (car mot) a) (eql (car mot) b))) f))))))

(defun ltrans (auto)
	(car auto))
(defun etati (auto) (caddr auto))


(defun etatf (auto) (cadr auto))


(defun etat-liste (auto)
	(if (null (ltrans auto)) '() (cons (caar (ltrans auto)) (etat-liste (list (cdr (ltrans auto)) (etatf auto) (etati auto))))))


(defun auto-finalp (auto e)
	(cond ((null (etatf auto)) '())
	      ((eql (car (etatf auto)) e) t)
	      (t (auto-finalp (list  (ltrans auto) (cdr (etatf auto)) (etati auto))))))   




(defun auto-tran-list (auto e)
	(cond ((null (ltrans auto)) '())
	      ((eql e (caar (ltrans auto))) (caddr ltrans)
	      (t (auto-tran(list  (ltrans auto) (cdr (etatf auto)) (etati auto)))))))       



(defun auto-compile (auto)
	(let ((etat-liste auto) l)
	(cond ((null l) `(lambda (mot) t))
	      '(labels (meta-auto (auto)) (ei auto (mot))))))
				)	




(defun meta-auto (auto)
	(let ((etat-liste auto) l))
	(cond ((null l) `(lambda (mot) t))
	      (t '(cons (compilox auto (car l)) (meta-auto (list (cdar auto) (cadr auto) (caddr auto)))))))  	



(defun compilox (auto e)
		(let ((ltrans auto e) l)
	(cond ((null l) `(e (mot) t))
		((auto-finalp auto e) '(e (mot auto) (let ((ltrans auto e) l)
			(cond ((null mot) '())
			((eql (car mot) (caar l)) (apply ((cdar l) (list (cdr mot) auto ))))
			 ((eql (car mot) (caadr mot)) (apply ((cdadr l)(list (cdr mot) auto))))
			 (t '())))))
		(t '(e (mot auto) (let ((ltrans auto e) l)
			(cond ((null mot) '())
			((eql (car mot) (caar l)) (apply ((cdar l) (list (cdr mot) auto ))))
			 ((eql (car mot) (caadr mot)) (apply ((cdadr l)(list (cdr mot) auto))))
			 (t '()))))))))
										 
			      