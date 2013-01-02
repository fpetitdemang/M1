(defun loader_vm (name expr)
  (loop for ligne in expr
	do
	(___loader___ name ligne)))

(defun ___loader___LABEL (name ligne etiq)
  (if (atom (gethash etiq (get_hash name 'ETIQUETTE)))
      (progn
	(setf (gethash etiq (get_hash name 'ETIQUETTE)) (get_register name 'Next_Free_Pos))
	(if (gethash etiq (get_hash name 'REF_AVANT))
	    (ref_avant_vm name ligne (gethash etiq (get_hash name 'REF_AVANT)))))))

(defun ___loader___JUMP (name ligne etiq saut)
  (progn
    (if (gethash etiq (get_hash name 'ETIQUETTE))
	(set_memory name (get_register name 'Next_Free_Pos) (list saut (gethash etiq (get_hash name 'ETIQUETTE))))

      (if (EQL (gethash etiq (get_hash name 'REF_AVANT)) NIL)
	  (progn 
	    (setf (gethash etiq (get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
	    (set_memory name (get_register name 'Next_Free_Pos) ligne))

	(progn
	  (setf (gethash etiq (get_hash name 'REF_AVANT)) (list (get_register name 'Next_Free_Pos)
								(gethash etiq (get_hash name 'REF_AVANT))))
	  (set_memory name (get_register name 'Next_Free_Pos) ligne))))

    (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1))))
  
(defun ___loader___LOAD (name ligne etiq load reg)
  (progn
    (if (gethash etiq (get_hash name 'ETIQUETTE))
	(set_memory name (get_register name 'Next_Free_Pos) (list load (gethash etiq (get_hash name 'ETIQUETTE)) reg))
	   
      (if (EQL (gethash etiq (get_hash name 'REF_AVANT)) NIL)
	  (progn
	    (setf (gethash etiq (get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
	    (set_memory name (get_register name 'Next_Free_Pos) ligne))
			      
	(progn
	  (setf (gethash etiq (get_hash name 'REF_AVANT))
		(list(get_register name 'Next_Free_Pos) (gethash etiq (get_hash name 'REF_AVANT)) reg))
	  (set_memory name (get_register name 'Next_Free_Pos) ligne))))

    (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1))))
  
(defun ___loader___STORE (name ligne etiq store reg)
  (progn
    (if (gethash etiq (get_hash name 'ETIQUETTE))
      	(set_memory name (get_register name 'Next_Free_Pos) (list store reg (gethash etiq (get_hash name 'ETIQUETTE))))
      
      (if (EQL (gethash(car (cdaddr ligne)) (get_hash name 'REF_AVANT)) NIL)
	  (progn
	    (setf (gethash etiq (get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
	    (set_memory name (get_register name 'Next_Free_Pos) ligne))
	
	(progn
	  (setf (gethash etiq (get_hash name 'REF_AVANT))
		(list(get_register name 'Next_Free_Pos) reg (gethash etiq (get_hash name 'REF_AVANT))))
	  (set_memory name (get_register name 'Next_Free_Pos) ligne))))
    
    (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1))))


(defun ___loader___ (name ligne)
  (if (EQL (car ligne) 'LABEL)
      (___loader___LABEL name ligne (cadr ligne))

    (if (AND (jumpp (car ligne))
	     (EQL (caadr ligne) '@))
	(___loader___JUMP name ligne (cadadr ligne) (car ligne))


      (if (AND (EQL 'LOAD (car ligne))
	       (listp (cadr ligne))
	       (EQL (caadr ligne) '@))
	  (___loader___LOAD name ligne (cadadr ligne) (car ligne) (caddr ligne))


	(if (AND (EQL 'STORE (car ligne))
		 (listp (caddr ligne))
		 (EQL (caaddr ligne) '@))
	    (___loader___STORE name ligne (car (cdaddr ligne)) (car ligne) (cadr ligne))

	  (progn
	    (if (integerp (car ligne))
		(set_memory name (get_register name 'Next_Free_Pos) (car ligne))
	      (set_memory name (get_register name 'Next_Free_Pos) ligne))
	    (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1))))))))


(defun ref_avant_vm (name ligne ref)
  (let ((refAvant ref))
    (loop while refAvant
	  do
	  (cond
	   ((listp refAvant)
	    (progn
	      (cond
	       ((EQ (car(get_memory name (car refAvant))) 'LOAD)
		(set_memory name (car refAvant) (list (car(get_memory name (car refAvant)))
						      (gethash (cadr ligne)(get_hash name 'ETIQUETTE))
						      (nth 2 (get_memory name (car refAvant))))))
	       ((EQ (car(get_memory name (car refAvant))) 'STORE)
		(set_memory name (car refAvant) (list (car(get_memory name (car refAvant)))
						     (nth 1 (get_memory name (car refAvant)))
						     (gethash (cadr ligne)(get_hash name 'ETIQUETTE)))))
	       (t
		(set_memory name (car refAvant) (list (car(get_memory name (car refAvant)))
						      (gethash (cadr ligne)(get_hash name 'ETIQUETTE))))))
	      (if (EQL(length refAvant) 1)
		  (setf refAvant (car refAvant))		
		(setf refAvant (cadr refAvant)))))
	   (T
	    (progn
	      (cond
	       ((EQ (car(get_memory name refAvant)) 'LOAD)
		(set_memory name refAvant (list (car(get_memory name refAvant))
						(gethash (cadr ligne)(get_hash name 'ETIQUETTE))
						(nth 2 (get_memory name refAvant)))))
	       ((EQ (car(get_memory name refAvant)) 'STORE)
		(set_memory name refAvant (list (car(get_memory name refAvant))
						(nth 1 (get_memory name refAvant))
						(gethash (cadr ligne)(get_hash name 'ETIQUETTE)))))
	       (t
		(set_memory name refAvant (list (car(get_memory name refAvant))
						(gethash (cadr ligne)(get_hash name 'ETIQUETTE))))))
	      (setf refAvant nil)))))))



