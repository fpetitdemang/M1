(defun membre(elmt l)
	(if (ATOM l)
		NIL 
	(if (= elmt (FIRST l))
		FIRST l
	(member elmt (cdr l)))))
	
(defun longueur (l)
	(if (ATOM l)
		0
	( + 1 (longueur(cdr l)))))
	
		
(defun derniere (l)
	(if (NULL (cdr l))
		(car l)
	(derniere(cdr l))))
	
(defun faireListeDecroi (n)
	(if (= n 0)
		()
	(cons n (faireListeDecroi (- n 1)))))
	
(defun faireListeCroi (n) ;refaire
	(if (= n 0)
		()
	(cons (faireListeCroi (- n 1)) n)))
	
(defun copieListe (l) 
	(if (ATOM l)
		l
	(cons (car l) (copieListe (cdr l)))))
	
;(defun supprimer (elmt x)

;	(if ATOM (= (car elmt) x)
			
;	)
	
