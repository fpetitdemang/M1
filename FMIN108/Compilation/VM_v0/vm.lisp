;//---------------------------------------------------// 
;//---------------------------------------------------// 
;//   MACHINE-VIRTUELLE  -->  Tp Lisp 2007/2008       //
;//---------------------------------------------------// 
;//---------------------------------------------------// 




;//********************************************************************************
;//********************************************************************************
;//
;//                        METHODES DE LA MACHINE VIRTUELLE   
;//
;//********************************************************************************
;/*********************************************************************************




;//--------------------------------------------------------------
;// Creation de la machine virtuelle : initialisation des données
;//--------------------------------------------------------------
(defun make_vm ( name sizeMemory )
    (progn
      ;on crée un tableau qui representera la memoire de notre machine virtuelle
      (setf (get name 'memory) (make-array sizeMemory :initial-element ()))
      ;création des registres 'RO 'R1 'R2
      (setf (get name 'R0) 0)
      (setf (get name 'R1) 0)
      (setf (get name 'R2) 0)
      ;pointe sur la base de la pile de memoire
      (setf (get name 'FP) 0)
      ;pointe sur le sommet de la pile de memoire
      (setf (get name 'SP) 0)
      ;represente le compteur ordinal (tete de lecture du code)
      (setf (get name 'CO) (- sizeMemory 1))
      (setf (get name 'Next_Free_Pos) (- (size_memory name) 1))
      ;creation des drapeaux de comparaison
      (setf (get name 'FL) 0)
      (setf (get name 'FE) 0)
      (setf (get name 'FG) 0)
      ;permet de gerer les etiquettes(LABELS) pour les sauts (JUMP)
      (setf (get name 'ETIQUETTE) (make-hash-table :size 0))
      (setf (get name 'REF_AVANT) (make-hash-table :size 0))
      ; permet de gerer l'arret de la machine
      (setf (get name 'RUN) nil)
      ; Permet le debugage quand la machine est en RUN
      (setf (get name 'DEBUG) nil)
      name))



;//------------------------------------------------------
;//  Reset de la machine virtuelle : réinitialisation
;//------------------------------------------------------
(defun reset_vm (name)
  (prog1
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
    ;cette boucle permet de vider la memoire
    (loop for pos from 0 to (- (size_memory name) 1)
      do
      (set_memory name pos () ))))

;//------------------------------------------------------
;//  Reset de la machine virtuelle : réinitialisation
;//------------------------------------------------------
(defun clean_vm (name)
  (prog1
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
    ;cette boucle permet de vider la memoire
    (loop for pos from 0 to (get_register name 'Next_Free_Pos)
      do
      (set_memory name pos () ))))


;//------------------------------------------------------
;//  Initialise la machine virtuelle : charge le chargeur
;//------------------------------------------------------
(defun init_vm(name)
  (reset_vm name)
  (loader_file_vm name "ASM_loader"))


;//-------------------------------------------------------------------------------
;// Applique le code passer en parametre au code charger dans la machine virtuelle
;//-------------------------------------------------------------------------------
(defun apply_vm (name expr DEBUG)
  (let ((pos (get_register name 'Next_Free_Pos)))
    ;On charge la partie du code d'ou la machine virtuelle demarrera
    (loader_vm name expr)
    ;On lance la machine virtuelle à la position souhaiter
    (run_vm name pos DEBUG)))
  


;//----------------------------------------------------------------------------------------
;// Chargement du code en memoire de la machine virtuelle à partir d'une liste d'expression
;//----------------------------------------------------------------------------------------
(defun loader_vm (name expr)
  (loop for ligne in expr
	do
	(___loader___ name ligne)))
      

;//---------------------------------------------------------------------------
;// Chargement du code en memoire de la machine virtuelle à partir d'un fihier
;//---------------------------------------------------------------------------
(defun loader_file_vm (name fichier)
  (labels ((lecture (o)
		    (let ((lu (read o nil nil nil)))
		      (loop while lu
			    do
			    (print lu)
			    (___loader___ name lu)
			    (setf lu (read o nil nil nil))))))
    (lecture (open fichier))))


;//****************************************************************************************
;//****************************************************************************************
;//
;//                        DEFINITIONS DES ACCESSEURS DE LA MACHINE   
;//
;//****************************************************************************************
;//****************************************************************************************



;//-------------------------
;// Accesseurs des registres 
;//-------------------------
(defun get_register ( name registre )
  (get name registre))

(defun set_register ( name registre valeur )
  (setf (get name registre) valeur))



;//--------------------------
;// Accesseurs sur la mémoire 
;//--------------------------
(defun get_memory (name case_memoire )
  (aref (get name 'memory) case_memoire))

(defun set_memory (name  case_memoire valeur )
  (setf (aref (get name 'memory) case_memoire) valeur))

(defun size_memory (name)
  (length (get name 'memory)))



;//----------------------------
;// Accesseurs sur la hashtable 
;//----------------------------
(defun get_hash ( name idTable )
  (get name idTable))



;//------------------------------------
;// Accesseurs sur l'état de la machine
;//------------------------------------
(defun get_etat ( name )
  (get name 'RUN))

(defun set_etat ( name valeur )
  (setf (get name 'RUN) valeur))

;//--------------------------------------
;// Accesseurs sur le DEBUG de la machine
;//--------------------------------------
(defun get_debug ( name )
  (get name 'DEBUG))

(defun set_debug ( name valeur )
  (setf (get name 'DEBUG) valeur))



;//****************************************************************************
;//****************************************************************************
;//
;//                        DEFINITIONS DES INSTRUCTIONS   
;//
;//****************************************************************************
;//****************************************************************************




;// Copie la valeur en memoire vers le registre de destination : (load 120 R0)
;// Ou la constante dans le registre de destination            : (load R1 R0)
;//-------------------------------------------------------------------------------
(defun LOAD_vm ( name lexpr )
  (cond 
   ; la source est un espace memoire
   ((integerp (car lexpr))
    (setf (get name (cadr lexpr)) (get_memory name (car lexpr))))
   ; la source est un registre
   ((registerp (car lexpr))
    (setf (get name (cadr lexpr)) (get_memory name (get_register (car lexpr)))))
   (t
    (setf (get name (cadr lexpr)) 
	  (get_memory name (+ (get_register name (cadar lexpr)) (caar lexpr))))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Copie la valeur du registre a la position de la memoire indiqué                  : (store R0 200)
;// Ou on recupere la valeur du registres de destination pour copier dans la memoire : (store R0 R1)
;//--------------------------------------------------------------------------------------------------
(defun STORE_vm ( name lexpr )
  (cond
   ;la dest est un registre
   ((registerp (cadr lexpr))
    (set_memory name (get_register name (cadr lexpr)) (get_register name (car lexpr))))
   ;la dest est un entier
   ((integerp (cadr lexpr))
    (set_memory name (cadr lexpr) (get_register name (car lexpr))))
    ; C'est une liste de la forme (-1 FP) qui recupere la valeur se trouvant a la position FP - 1 dans la memoire
   (t
    (set_memory name (+ (get_register name (cadadr lexpr)) (caadr lexpr)) (get_register name (car lexpr)))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Seulement entre registre : (move R0 R1)
;//----------------------------------------
(defun MOVE_vm ( name lexpr )
  (cond 
   ; la source est un registre
   ((registerp (car lexpr))
    (setf (get name (cadr lexpr)) (get name (car lexpr))))
  
   ; la source est une constante
   ((EQL (caar lexpr) '$)
    (setf (get name (cadr lexpr)) (cadar lexpr)))
   
   ; C'est une liste de la forme (-1 FP) qui recupere la valeur se trouvant a la position FP - 1 dans la memoire
   (t
    (setf (get name (cadr lexpr)) (get_memory name (+ (get_register name (cadar lexpr)) (caar lexpr))))))

  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Copie la source à la case memoire pointé par SP
;//------------------------------------------------
(defun PUSH_vm ( name registre )
  (if (= (get_register name 'SP) (get_register name 'CO))
      (error "[PUSH_vm] Erreur le code se trouve sur la pile")
    (prog1
      (set_memory name (get_register name 'SP) (get_register name registre))
      (set_register name 'SP (+ (get_register name 'SP) 1))
      (set_register name 'CO (- (get_register name 'CO) 1))
      )))



;// Charge dans le registre le contenu de la case memoire pointé par SP
;//--------------------------------------------------------------------
(defun POP_vm ( name registre )
  (if (= (get_register name 'SP) (get_register name 'FP))
      (error "[POP_vm] Impossible de dépiler")
    (progn 
      (set_register name 'SP      (- (get_register name 'SP) 1))
      (set_register name registre ( get_memory name (get_register name 'SP)))
      (set_register name 'CO      (- (get_register name 'CO) 1 )))))



;// Seulement sur les registres : (INCR R0)
;//----------------------------------------
(defun INCR_vm (name registre)
  (if (not (registerp registre))
      (error "[INCR_vm] INCR seulement pour les registres")
    (set_register name registre (+ (get_register name registre) 1)))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Seulement sur les registres : (DECR R0)
;//----------------------------------------
(defun DECR_vm (name registre)
  (if (not (registerp registre))
      (error "[DECR_vm] DECR seulement pour les registres")
    (set_register name registre (- (get_register name registre) 1)))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// La multiplication : (MULT R0 R1)
;// R1 = R1 * R0
;//---------------------------------
(defun MUL_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(* (get_register name (cadr lexpr)) (get_register name (car lexpr)))) 
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// L'addition : (ADD R0 R1)
;// R1 = R1 + R0
;//---------------------------------
(defun ADD_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(+ (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// La division : (DIV R0 R1)
;// R1 = R1 / R0
;//---------------------------------
(defun DIV_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(/ (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// La soustraction : (SUB R0 R1)
;// R1 = R1 - R0
;//---------------------------------
(defun SUB_vm ( name lexpr )
  (set_register name (cadr lexpr)
		(- (get_register name (cadr lexpr)) (get_register name (car lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Comparaison entre deux elements
;//--------------------------------
(defun CMP_vm ( name lexpr )

	
  ; Il s'agit d'une comparaison classique entre deux elements par l'intermediaire d'un operateur de comparaison
  ;(print "**********************")
  ;(print "**********************")
  ;(print lexpr)
  ;(if (EQL (length lexpr) 2)
  ;    (progn
  ;	(print (get_register name (car lexpr)))
  ;	(print (get_register name (cadr lexpr)))
  ;	(print (EQL (get_register name (car lexpr)) (get_register name (cadr lexpr))))
  ;	))
  ;  (print "**********************")
  ;  (print "**********************")
  ;  (read-char)

  (if (EQL (length lexpr) 2) 
      (cond 
       ((EQL (get_register name (car lexpr)) (get_register name (cadr lexpr)))
	(progn 
	  (setf (get name 'FE) 1)
	  (setf (get name 'FG) 0)
	  (setf (get name 'FL) 0))) 
	
       ((AND
	 (integerp (get_register name (car lexpr)))
	 (integerp (get_register name (cadr lexpr))))
	 
	(cond
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
	    (setf (get name 'FL) 0)))))
       (t
	(progn 
	  (setf (get name 'FE) 0)
	  (setf (get name 'FG) 0)
	  (setf (get name 'FL) 0))))
    
    ; Il s'agit d'une comparaison par fonction prédefinie (consp atom ...)
    (if (get_register name (car lexpr))
	(progn 
	  (setf (get name 'FE) 1)
	  (setf (get name 'FG) 0)
	  (setf (get name 'FL) 0))
      (progn 
	(setf (get name 'FE) 0)
	(setf (get name 'FG) 0)
	(setf (get name 'FL) 0))))
	
  (set_register name 'CO (- (get_register name 'CO) 1)))
 


;// Le saut jump
;//-------------
(defun JMP_vm ( name etiquette )
  (set_register name 'CO etiquette))



;// Le saut jsr
;//------------
(defun JSR_vm ( name etiquette )
  (set_memory name (get_register name 'SP) (- (get_register name 'CO) 1))
  (set_register name 'SP (+ (get_register name 'SP) 1))
  (if (listp etiquette)
      (OTHER_vm name (cadr etiquette))
    (JMP_vm name etiquette)))



;// Le saut jne
;//------------
(defun JNE_vm ( name etiquette )
  (if (EQL (get_register name 'FE) 0) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le saut jeq
;//------------
(defun JEQ_vm ( name etiquette )
  (if (EQL (get_register name 'FE) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le saut jl
;//-----------
(defun JL_vm ( name etiquette )
  (if (EQL (get_register name 'FL) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le saut jle
;//------------
(defun JLE_vm ( name etiquette )
  (if (or
       (EQL (get_register name 'FE) 1)
       (EQL (get_register name 'FL) 1))    
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le saut jg
;//-----------
(defun JG_vm ( name etiquette )
  (if (EQL (get_register name 'FG) 1) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le saut jge
;//------------
(defun JGE_vm ( name etiquette )
  (if (or
       (EQL (get_register name 'FE) 1)
       (EQL (get_register name 'FG) 1)) 
      (set_register name 'CO etiquette)
    (set_register name 'CO (- (get_register name 'CO) 1))))



;// Le rtn
;//-------
(defun RTN_vm (name)
  (if (< (- (get_register name 'SP) 1) 0)
      (error "[RTN_vm] Erreur")
    (progn
      (set_register name 'SP (- (get_register name 'SP) 1))
      (set_register name 'CO (get_memory name (get_register name 'SP))))))



;// Le nop
;//-------
(defun NOP_vm (name)
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Arret de la machine lecture d'un HALT
;//--------------------------------------
(defun HALT_vm (name)
  (set_etat name nil))

;// Seulement sur les registres : (CAR R0)
;//----------------------------------------
(defun CAR_vm (name lregistre)
  (set_register name (cadr lregistre) (car (get_register name (car lregistre))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Seulement sur les registres : (CDR R0)
;//----------------------------------------
(defun CDR_vm (name lregistre)
  (set_register name (cadr lregistre) (cdr (get_register name (car lregistre))))
  (set_register name 'CO (- (get_register name 'CO) 1)))


;// Appel du setf contenant un get
;//-------------------------------
(defun SETFGET_vm (name lexpr)
  (set_register name (caddr lexpr)
  		(setf (get (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Appel du setf contenant un gethash
;//---------------------------------
(defun SETFHASH_vm (name lexpr)  
  (set_register name (caddr lexpr)
  		(setf (gethash (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Appel du setf contenant un aref
;//--------------------------------
(defun SETFAREF_vm (name lexpr)  
  (set_register name (caddr lexpr)
  		(setf (aref (get_register name (car lexpr)) (get_register name (cadr lexpr))) (get_register name (caddr lexpr))))
  (set_register name 'CO (- (get_register name 'CO) 1)))



;// Les autres
;//-----------
(defun OTHER_vm ( name lexpr )
  (let ((liste (___OTHER___ name (get_memory name (- (get_register name 'FP) 1)) ())))
    (setf (get name 'R0) (apply lexpr liste))
    (RTN_vm name)))



;//************************************************************************************
;//************************************************************************************
;//
;//                        DEFINITIONS DE METHODES AUXILLIAIRES   
;//
;//************************************************************************************
;//************************************************************************************



;//--------------------------------------------------------------------------------
;// Chargement du code en memoire de la machine virtuelle expression par expression
;//--------------------------------------------------------------------------------
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


;(defun ___loader___ (name ligne)
;  (cond
;   ((AND (EQL (car ligne) 'LABEL)
;	 (atom (gethash (cadr ligne) (get_hash name 'ETIQUETTE))))
;    (progn
;      (setf (gethash (cadr ligne) (get_hash name 'ETIQUETTE)) (get_register name 'Next_Free_Pos))
;      (if (gethash (cadr ligne)(get_hash name 'REF_AVANT))
;	  (ref_avant_vm name ligne (gethash(cadr ligne)(get_hash name 'REF_AVANT))))))
;
;   ((AND (jumpp (car ligne))
;	 (EQL (caadr ligne) '@))
;    (cond
;     ((gethash (cadadr ligne) (get_hash name 'ETIQUETTE))
;      (set_memory name (get_register name 'Next_Free_Pos) (list (car ligne)
;							       (gethash (cadadr ligne) (get_hash name 'ETIQUETTE))))
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;     
;     ((EQL (gethash(cadadr ligne)(get_hash name 'REF_AVANT)) NIL)
;      (setf (gethash(cadadr ligne)(get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;    
;     (t
;      (setf (gethash(cadadr ligne)(get_hash name 'REF_AVANT))
;	    (list(get_register name 'Next_Free_Pos)
;		 (gethash(cadadr ligne)(get_hash name 'REF_AVANT))))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))))
;
;   ((AND (EQL 'LOAD (car ligne))
;	 (listp (cadr ligne))
;	 (EQL (caadr ligne) '@))
;    (cond
;     ((gethash (cadadr ligne) (get_hash name 'ETIQUETTE))
;      (set_memory name (get_register name 'Next_Free_Pos) (list (car ligne)
;								(gethash (cadadr ligne) (get_hash name 'ETIQUETTE))
;								(caddr ligne)))
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;     
;     ((EQL (gethash(cadadr ligne)(get_hash name 'REF_AVANT)) NIL)
;      (setf (gethash(cadadr ligne)(get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;     
;     (t
;      (setf (gethash(cadadr ligne)(get_hash name 'REF_AVANT))
;	    (list(get_register name 'Next_Free_Pos)
;		 (gethash(cadadr ligne)(get_hash name 'REF_AVANT))
;		 (caddr ligne)))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))))
;
;   ((AND (EQL 'STORE (car ligne))
;	 (listp (caddr ligne))
;	 (EQL (caaddr ligne) '@))
;    (cond
;     ((gethash (car (cdaddr ligne)) (get_hash name 'ETIQUETTE))
;      (set_memory name (get_register name 'Next_Free_Pos) (list (car ligne)
;								(cadr ligne)
;								(gethash (car (cdaddr ligne)) (get_hash name 'ETIQUETTE))))
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;			  
;     ((EQL (gethash(car (cdaddr ligne)) (get_hash name 'REF_AVANT)) NIL)
;      (setf (gethash(car (cdaddr ligne))(get_hash name 'REF_AVANT)) (get_register name 'Next_Free_Pos))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))
;
;     (t
;      (setf (gethash(car (cdaddr ligne))(get_hash name 'REF_AVANT))
;	    (list(get_register name 'Next_Free_Pos)
;		 (cadr ligne)
;		 (gethash(car (cdaddr ligne))(get_hash name 'REF_AVANT))))
;      (set_memory name (get_register name 'Next_Free_Pos) ligne)
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1)))))
;	
;   (t
;    (progn
;      (if (integerp (car ligne))
;	  (set_memory name (get_register name 'Next_Free_Pos) (car ligne))
;	(set_memory name (get_register name 'Next_Free_Pos) ligne))
;      (set_register name 'Next_Free_Pos (- (get_register name 'Next_Free_Pos) 1))))))



;//------------------------------------------------------------------------
;// Evaluation d'une expression lors de l'execution de la machine virtuelle
;//------------------------------------------------------------------------
(defun eval_vm ( name expression )
  (case (car expression)
    (STORE     (STORE_vm     name   ( cdr  expression )))
    (LOAD      (LOAD_vm      name   ( cdr  expression )))
    (MOVE      (MOVE_vm      name   ( cdr  expression ))) 
    (PUSH      (PUSH_vm      name   ( cadr expression )))
    (POP       (POP_vm       name   ( cadr expression )))
    (INCR      (INCR_vm      name   ( cadr expression )))
    (DECR      (DECR_vm      name   ( cadr expression )))
    (CAR       (CAR_vm       name   ( cdr  expression )))
    (CDR       (CDR_vm       name   ( cdr  expression )))
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



;//------------------------------------------------------------------------
;// Execution de la machine virtuelle : execution du code chargé en memoire
;//------------------------------------------------------------------------
(defun run_vm (name position DEBUG)
  (if (> position 0)
      ;le compteur ordinal est placé la ou on veut qui demarre
      (set_register name 'CO position)
    ;le compteur ordinal est placé sur la premiere expression en memoire
    (set_register name 'CO (- (size_memory name) 1)))
  ; On met a jour le DEBUG
  (set_debug name DEBUG)
  ;l'état de la machine est RUNNING
  (set_etat name t )
  
  (loop while (get_etat name)
	do
	(if (get_debug name)
	    (progn 
	      (print "----------------------------")
	      (print "Expression :" )
	      (princ (get_memory name (get_register name 'CO)))))

        ; evaluation de cette expression
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
  

;//-----------------------------------------------------------------
;// S'occupe des ref en avants lors du chargement du code dans la vm
;//-----------------------------------------------------------------
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



;//------------------------------------------------------------------
;// Retourne une liste a partir des parametres chargé dans la memoire
;//------------------------------------------------------------------
(defun ___OTHER___ ( name nbParam liste)
  (if (EQL (length liste) nbParam)
      liste
    (___OTHER___ name nbParam (cons (get_memory name (- (get_register name 'FP) (+ (length liste) 2))) liste))))



;//----------------------------------------------------------------
;// Affiche tout le code chargé en memoire jusqu'a le valeur choisi
;//----------------------------------------------------------------
(defun print_vm (name val)
  (loop for pos from 0 to (- val 1)
	do
	(print pos)
	(princ " : ")
	(prin1 (get_memory name pos))))



;//----------------------------------------
;// Est ce que l'espression est un registre 
;//----------------------------------------
(defun registerp (expression)
  (member expression '(R0 R1 R2 FP SP FE FG FL CO)))



;//------------------------------------
;// Est ce que l'espression est un saut
;//------------------------------------
(defun jumpp (expression)
  (member expression '(jmp jsr jeq jneq jg jl jge jle)))
