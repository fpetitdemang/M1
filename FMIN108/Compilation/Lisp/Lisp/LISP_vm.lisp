(defun make_vm ( name sizeMemory )
    (progn
      (setf (get name 'memory) (make-array sizeMemory :initial-element ()))
      (setf (get name 'R0) 0)
      (setf (get name 'R1) 0)
      (setf (get name 'R2) 0)
      (setf (get name 'FP) 0)
      (setf (get name 'SP) 0)
      (setf (get name 'CO) (- sizeMemory 1))
      (setf (get name 'Next_Free_Pos) (- (size_memory name) 1))
      (setf (get name 'FL) 0)
      (setf (get name 'FE) 0)
      (setf (get name 'FG) 0)
      (setf (get name 'ETIQUETTE) (make-hash-table :size 0))
      (setf (get name 'REF_AVANT) (make-hash-table :size 0))
      (setf (get name 'RUN) nil)
      (setf (get name 'DEBUG) nil)
      name))

(defun reset_vm (name)
  (progn
    (setf (get name 'R0) 0)
    (setf (get name 'R1) 0)
    (setf (get name 'R2) 0)
    (setf (get name 'FP) 0)
    (setf (get name 'SP) 0)
    (setf (get name 'FL) 0)
    (setf (get name 'FE) 0)
    (setf (get name 'FG) 0)
    (setf (get name 'CO) (- (size_memory name) 1))
    (setf (get name 'Next_Free_Pos) (- (size_memory name) 1))
    (setf (get name 'ETIQUETTE) (make-hash-table :size 0))
    (setf (get name 'REF_AVANT) (make-hash-table :size 0))
    (setf (get name 'ETAT) 'STOP )
    (setf (get name 'RUN) nil)
    (setf (get name 'DEBUG) nil)
    (loop for pos from 0 to (- (size_memory name) 1)
      do
      (set_memory name pos () ))))

(defun clean_vm (name)
  (progn
    (setf (get name 'R0) 0)
    (setf (get name 'R1) 0)
    (setf (get name 'R2) 0)
    (setf (get name 'FP) 0)
    (setf (get name 'SP) 0)
    (setf (get name 'FL) 0)
    (setf (get name 'FE) 0)
    (setf (get name 'FG) 0)
    (setf (get name 'ETAT) 'STOP )
    (setf (get name 'RUN) nil)
    (setf (get name 'DEBUG) nil)
    (loop for pos from 0 to (get_register name 'Next_Free_Pos)
      do
      (set_memory name pos () ))))

(defun apply_vm (name expr DEBUG)
  (let ((pos (get_register name 'Next_Free_Pos)))
    (loader_vm name expr)
    (run_vm name pos DEBUG)))

(defun get_register ( name registre )
  (get name registre))

(defun set_register ( name registre valeur )
  (setf (get name registre) valeur))

(defun get_memory (name case_memoire )
  (aref (get name 'memory) case_memoire))

(defun set_memory (name  case_memoire valeur )
  (setf (aref (get name 'memory) case_memoire) valeur))

(defun size_memory (name)
  (length (get name 'memory)))

(defun get_hash ( name idTable )
  (get name idTable))

(defun get_etat ( name )
  (get name 'RUN))

(defun set_etat ( name valeur )
  (setf (get name 'RUN) valeur))

(defun get_debug ( name )
  (get name 'DEBUG))

(defun set_debug ( name valeur )
  (setf (get name 'DEBUG) valeur))

(defun LOAD_vm ( name lexpr )
  (cond 
   ((integerp (car lexpr))
    (setf (get name (cadr lexpr)) (get_memory name (car lexpr))))
   ((registerp (car lexpr))
    (setf (get name (cadr lexpr)) (get_memory name (get_register (car lexpr))))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun STORE_vm ( name lexpr )
  (cond
   ((registerp (cadr lexpr))
    (set_memory name (get_register name (cadr lexpr)) (get_register name (car lexpr))))
   ((integerp (cadr lexpr))
    (set_memory name (cadr lexpr) (get_register name (car lexpr))))
   (t
    (set_memory name (+ (get_register name (cadadr lexpr)) (caadr lexpr)) (get_register name (car lexpr)))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun MOVE_vm ( name lexpr )
  (cond 
   ((registerp (car lexpr))
    (setf (get name (cadr lexpr)) (get name (car lexpr))))
   ((EQL (caar lexpr) '$)
    (setf (get name (cadr lexpr)) (cadar lexpr)))
   (t
    (setf (get name (cadr lexpr)) (get_memory name (+ (get_register name (cadar lexpr)) (caar lexpr))))))

  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun PUSH_vm ( name registre )
  (if (= (get_register name 'SP) (get_register name 'CO))
      (error "[PUSH_vm] Erreur le code se trouve sur la pile")
    (prog1
      (set_memory name (get_register name 'SP) (get_register name registre))
      (set_register name 'SP (+ (get_register name 'SP) 1))
      (set_register name 'CO (- (get_register name 'CO) 1))
      )))

(defun POP_vm ( name registre )
  (if (= (get_register name 'SP) (get_register name 'FP))
      (error "[POP_vm] Impossible de dépiler")
    (progn 
      (set_register name 'SP      (- (get_register name 'SP) 1))
      (set_register name registre ( get_memory name (get_register name 'SP)))
      (set_register name 'CO      (- (get_register name 'CO) 1 )))))

(defun INCR_vm (name registre)
  (if (not (registerp registre))
      (error "[INCR_vm] INCR seulement pour les registres")
    (set_register name registre (+ (get_register name registre) 1)))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun DECR_vm (name registre)
  (if (not (registerp registre))
      (error "[DECR_vm] DECR seulement pour les registres")
    (set_register name registre (- (get_register name registre) 1)))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun MUL_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(* (get_register name (cadr lexpr)) (get_register name (car lexpr)))) 
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun ADD_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(+ (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun DIV_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(/ (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun SUB_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(- (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun CMP_vm ( name lexpr )
  (cond
   ((EQL (length lexpr) 1)
    (if (get_register name (car lexpr))
	(progn 
	  (setf (get name 'FE) 1)
	  (setf (get name 'FG) 0)
	  (setf (get name 'FL) 0))
      (progn 
	(setf (get name 'FE) 0)
	(setf (get name 'FG) 0)
	(setf (get name 'FL) 0))))
   
   ((EQL (get_register name (car lexpr)) (get_register name (cadr lexpr)))
    (progn 
      (setf (get name 'FE) 1)
      (setf (get name 'FG) 0)
      (setf (get name 'FL) 0)))
  
   (( < (get_register name (car lexpr)) (get_register name (cadr lexpr)))
    (progn 
      (setf (get name 'FE) 0)
      (setf (get name 'FG) 0)
      (setf (get name 'FL) 1)))
   (( > (get_register name (car lexpr)) (get_register name (cadr lexpr)))
    (progn 
      (setf (get name 'FE) 0)
      (setf (get name 'FG) 1)
      (setf (get name 'FL) 0)))
   (t
    (progn 
      (setf (get name 'FE) 0)
      (setf (get name 'FG) 0)
      (setf (get name 'FL) 0))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun JMP_vm ( name etiquette )
  (set_register name 'CO etiquette))

(defun JSR_vm ( name etiquette )
  (set_memory name (get_register name 'SP) (- (get_register name 'CO) 1))
  (set_register name 'SP (+ (get_register name 'SP) 1))
  (if (listp etiquette)
      (OTHER_vm name (cadr etiquette))
    (JMP_vm name etiquette)))

(defun JNE_vm ( name etiquette )
  (if (EQL (get_register name 'FE) 0) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun JEQ_vm ( name etiquette )
  (if (EQL (get_register name 'FE) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun JL_vm ( name etiquette )
  (if (EQL (get_register name 'FL) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun JLE_vm ( name etiquette )
  (if (or
       (EQL (get_register name 'FE) 1)
       (EQL (get_register name 'FL) 1))    
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun JG_vm ( name etiquette )
  (if (EQL (get_register name 'FG) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun JGE_vm ( name etiquette )
  (if (or
       (EQL (get_register name 'FE) 1)
       (EQL (get_register name 'FG) 1)) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))

(defun RTN_vm (name)
  (if (< (- (get_register name 'SP) 1) 0)
      (error "[RTN_vm] Erreur")
    (progn
      (set_register name 'SP (- (get_register name 'SP) 1))
      (set_register name 'CO (get_memory name (get_register name 'SP))))))

(defun NOP_vm (name)
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun HALT_vm (name)
  (set_etat name nil))

(defun SETFGET_vm (name lexpr)
  (set_register name (caddr lexpr)
  		(setf (get (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun SETFHASH_vm (name lexpr)  
  (set_register name (caddr lexpr)
  		(setf (gethash (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun SETFAREF_vm (name lexpr)  
  (set_register name (caddr lexpr)
  		(setf (aref (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))

(defun OTHER_vm ( name lexpr )
  (let ((liste (___OTHER___ name (get_memory name (- (get_register name 'FP) 1)) ())))
    (setf (get name 'R0) (apply lexpr liste))
    (RTN_vm name)))


(defun eval_vm ( name expression )
  (case (car expression)
    (STORE     (STORE_vm     name   ( cdr  expression )))
    (LOAD      (LOAD_vm      name   ( cdr  expression )))
    (MOVE      (MOVE_vm      name   ( cdr  expression ))) 
    (PUSH      (PUSH_vm      name   ( cadr expression )))
    (POP       (POP_vm       name   ( cadr expression )))
    (INCR      (INCR_vm      name   ( cadr expression )))
    (DECR      (DECR_vm      name   ( cadr expression )))
    (ADD       (ADD_vm       name   ( cdr  expression )))
    (SUB       (SUB_vm       name   ( cdr  expression )))
    (MUL       (MUL_vm       name   ( cdr  expression )))
    (DIV       (DIV_vm       name   ( cdr  expression )))
    (CMP       (CMP_vm       name   ( cdr  expression )))
    (JSR       (JSR_vm       name   ( cadr expression )))
    (JMP       (JMP_vm       name   ( cadr expression )))
    (JEQ       (JEQ_vm       name   ( cadr expression )))
    (JNEQ      (JNE_vm       name   ( cadr expression )))
    (JLE       (JLE_vm       name   ( cadr expression )))
    (JGE       (JGE_vm       name   ( cadr expression )))
    (JL        (JL_vm        name   ( cadr expression )))
    (JG        (JG_vm        name   ( cadr expression )))
    (SETFGET   (SETFGET_vm   name   ( cdr  expression )))
    (SETFHASH  (SETFHASH_vm  name   ( cdr  expression )))
    (SETFAREF  (SETFAREF_vm  name   ( cdr  expression )))
    (HALT      (HALT_vm      name   ))
    (NOP       (NOP_vm       name   ))
    (RTN       (RTN_vm       name   ))))

(defun run_vm (name position DEBUG)
  (if (> position 0)
      (set_register name 'CO position)
    (set_register name 'CO (- (size_memory name) 1)))
  (set_debug name DEBUG)
  (set_etat name t )
  
  (loop while (get_etat name)
	do
	(if (get_debug name)
	    (progn 
	      (print "----------------------------")
	      (print "Expression :" )
	      (princ (get_memory name (get_register name 'CO)))))

	(eval_vm name (get_memory name (get_register name 'CO)))
      
	(if (get_debug name)
	    (progn 
	      (print "Valeur de R0 :" )
	      (princ (get_register name 'R0))
	      (print "Valeur de R1 :" )
	      (princ (get_register name 'R1))
	      (print "Valeur de R2 :" )
	      (princ (get_register name 'R2))
	      (print "Valeur de SP :" )
	      (princ (get_register name 'SP))
	      (print "Valeur de FP :" )
	      (princ (get_register name 'FP))
	      (print "Valeur de FE :" )
	      (princ (get_register name 'FE))
	      (print "Valeur de FL :" )
	      (princ (get_register name 'FL))
	      (print "Valeur de FG :" )
	      (princ (get_register name 'FG))
	      (print "Valeur de CO :" )
	      (princ (get_register name 'CO))
	      (print "Valeur de Next_Free_Pos :" )
	      (princ (get_register name 'Next_Free_Pos))
	      (print_vm name (get_register name 'SP))
	      (print "----------------------------")
	      (read-char)
	      )))

  (if (EQL (get_etat name) nil)
      (progn
  	(print "Resultat :" )
  	(princ (get_register name 'R0)))))


(defun ___OTHER___ ( name nbParam liste)
  (if (EQL (length liste) nbParam)
      liste
    (___OTHER___ name nbParam (cons (get_memory name (- (get_register name 'FP) (+ (length liste) 2))) liste))))

(defun print_vm (name val)
  (loop for pos from 0 to val
	do
	(print pos)
	(princ " : ")
	(prin1 (get_memory name pos))))

(defun registerp (expression)
  (member expression '(R0 R1 R2 FP SP FE FG FL CO)))

(defun jumpp (expression)
  (member expression '(jmp jsr jeq jneq jg jl jge jle)))

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




