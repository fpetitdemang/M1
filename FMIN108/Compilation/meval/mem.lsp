(defvar TailleMemoire 65536)

(defvar Memoire (make-array (+ 1 TailleMemoire))) ;64 kilo-liste. La case 0 est réservée. La mémoire est donc 65535 fois infinie !

(defvar TaillePile 16384) ; 16 kilo-liste de pile

(defvar Dernier-Code-Charge 1) ;Variable indiquant la position de fin du dernier code chargé. Ne prends pas en compte les codes d'appels

(mdefun Memoire (indice) ;Accesseur en lecture
  (elt Memoire indice))

(defvar Table-liens (make-hash-table)) ;Table pour gérer les adresses et leur résolution.

(mdefun Ecris-Memoire (indice valeur) ;Accesseur en écriture
  (setf (elt Memoire indice) valeur))

;;;;GESTION DE LA PILE;;;;;;;;
(mdefun Mem-push (valeur) 
 (if (> (retourne-valeur 'SP) (- TailleMemoire TaillePile))
      (progn (setf (gethash 'SP registres) (1- (retourne-valeur 'SP))) 
	     (setf (elt Memoire (retourne-valeur 'SP)) valeur))
    (error "push: pile pleine"))
)

(mdefun Mem-pop ()
 (if (<= (retourne-valeur 'SP) (- TailleMemoire 1))
      (progn (setf (gethash 'SP registres) (1+ (retourne-valeur 'SP)))
	     (elt Memoire (1- (retourne-valeur 'SP))))
    (error "pop: pile vide"))
)



(mdefun retourne-pile (indice)
  (Memoire (+ (gethash 'fp registres) indice)))

(mdefun affecte-pile ( indice valeur)
  (Ecris-Memoire (+ (gethash 'fp registres) indice) valeur))




;La résolution des adresses en mémoire n'aura lieu que lorsqu'on demandera le 
;chargement d'un code d'appel.

;Ne pas utiliser pour charger le code d'appel !

(mdefun Charge-Code (liste-instructions etiquette-appel)
  (labels 
   (
    
    
    (nb-etiq (l) ;compte les étiquettes
	     (let ((i 0) (fin (length l)) (nb-e 0))
	       (loop while (< i fin) do
		     (if (atom (elt l i))
			 (setf nb-e (+ nb-e 1)))
		     (setf i (+ 1 i))
		     )
	       nb-e))

	   (nb-pseudo-instructions (l) ;Pour l'instant ne compte que les defglobal
				   (let ((i 0) (fin (length l)) (nb-psi 0))
				     (loop while (< i fin) do
					   (if (and (not (atom (elt l i))) (eq (car (elt l i)) 'DEFGLOBAL))
					       (setf nb-psi (+ nb-psi 1)))
					   (setf i (+ 1 i)))
				     nb-psi))

	   (aux2 (l dc)
		 (let ((i 0) (j dc) (ips -1) (instruction-courante nil) (FC (+ (length l) (- dc (nb-etiq l) (nb-pseudo-instructions l)))))
		       (loop while (< j FC) do
			     (setf instruction-courante (copy-tree (elt l i))) ;Copy-tree est nécessaire pour que les adresses ne soient pas résolues dans l'environnement du compilateur
			     (if (atom instruction-courante) ;C'est une étiquette
				 (progn 
				   (setf (gethash instruction-courante Table-liens) j)
				   (setf i (+ 1 i)))
			       (if (eq (car instruction-courante) 'DEFGLOBAL)
				   (progn
				     (setf ips (+ 1 ips))
				     (setf (gethash (cadr instruction-courante) variables-globales) (+ FC ips ))
				     (setf liste-variables-globales (cons (+ FC ips) liste-variables-globales))
				     (ecris-memoire (+ FC ips) (caddr instruction-courante))
				     (setf i (+ 1 i)))
				 
				 (progn
				   (Ecris-Memoire j instruction-courante)
				   (setf i (+ 1 i))
				   (setf j (+ 1 j)))

				 )
			       ))))
	   )
    (progn
      (setf (gethash etiquette-appel Table-liens) Dernier-Code-Charge) ;Stocke l'adresse de chargement pour la résolution des appels
      (aux2 liste-instructions Dernier-Code-Charge) ;Charge le code en memoire
;      
      (setf Dernier-Code-Charge (- Dernier-Code-Charge (nb-etiq liste-instructions)))
     
      (setf Dernier-Code-Charge (+ Dernier-Code-Charge (length liste-instructions)))
      
      )
    )
  )



(mdefun resoud-adresse-saut-simple (op1 instruction)
  (let ((adresse (gethash op1 Table-liens)))
    (if (not (numberp op1))                 ;Si l'adresse n'est pas déjà résolue !
	(if (null adresse)
	    instruction
	  (setf (car (cdr instruction))  adresse)
	  )
      instruction
      )
    )
  )
  
(mdefun resoud-adresse-saut-conditionnel (op3 instruction)
  (let ((adresse (gethash op3 Table-liens)))
    (if (not (numberp op3))             ;idem !
	(if (null adresse)
	    instruction ;ADRESSE NON DEFINIE CLISP DECLENCHERA L'ERREUR
	  (setf (car (cdddr instruction))  adresse)
	  )
      instruction ; APPEL D'UNE FONCTION CLISP
      )
    )
  )

(mdefun resoud-adresse-global (op2 instruction)
	(let ((adresse (gethash op2 variables-globales)))
	  (if (not (numberp op2))
	      (if (null adresse)
		  instruction
		(setf (car (cddr instruction)) adresse)
		)
	    instruction
	    )
	  )
	)

(mdefun resoud-adresse-setglobal (op1 instruction)
	(let ((adresse (gethash op1 variables-globales)))
	  (if (not (numberp op1))
	      (if (null adresse)
		  instruction
		  (setf (car (cdr instruction)) adresse))
	    instruction
	    )))


;Resouds l'adresse sur une instruction
(mdefun resoud-adresse (instruction)
  (let ((op0 (car instruction))
	(op1 (cadr instruction))
	(op2 (caddr instruction))
	(op3 (cadddr instruction))
	)
    (progn
      (case op0
	(JMP (resoud-adresse-saut-simple op1 instruction))
	(JSR (resoud-adresse-saut-simple op1 instruction))
	(JT  (resoud-adresse-saut-simple op1 instruction))
	(JF  (resoud-adresse-saut-simple op1 instruction))

	(JEQ (resoud-adresse-saut-conditionnel op3 instruction))
	(JNE (resoud-adresse-saut-conditionnel op3 instruction))
	(JL  (resoud-adresse-saut-conditionnel op3 instruction))
	(JLE (resoud-adresse-saut-conditionnel op3 instruction))  
	(JG  (resoud-adresse-saut-conditionnel op3 instruction))
	(JGE (resoud-adresse-saut-conditionnel op3 instruction))
	(GLOBAL (resoud-adresse-global op2 instruction))
	(SETGLOBAL (resoud-adresse-setglobal op1 instruction))
	
	)
      instruction
      )
)) 




;;;;Attention !!!!!!!!!!!!!!
;; Meme si l'appel ne contient qu'une instruction il doit etre une liste d'une instruction !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mdefun lance-code-appel (liste-instruction &optional (debug t))
  (let ((i 1) (fin-code (+ Dernier-code-charge  (length liste-instruction))) (instruction-courante nil))
    (labels ((aux(l i)
		 (if (consp l) 
		     (if (atom (car l)) ;C'est une étiquette
			 (progn 
			   (setf (gethash (car l)  Table-liens) i)
			   (aux (cdr l) i))
		       
		       (progn
			 (Ecris-Memoire i (copy-tree (car l)))
			 (aux (cdr l) ( + i 1)))
		       )
		   )
		 )
	     
	     
	     )
	    
	    
	    (aux liste-instruction Dernier-Code-Charge)
	    

	    (loop while (< i fin-code) do
					
		  (setf instruction-courante (Memoire i))
					
		  (if (not (member i liste-variables-globales))
		      (Ecris-Memoire i (resoud-adresse instruction-courante)))
					
		  (setf i (+ 1 i))
		  )
		
		(Ecris-Memoire fin-code '(end))
	    (vm-eval Dernier-Code-Charge debug)
	    
	    (retourne-valeur 'A0)
	    )))

(mdefun affiche-ram (debut fin)
  (let ((i debut))
    (loop while (<= i fin) do
      (format *standard-output* "~s =====> ~s~%" i (memoire i))
      (setf i (+ 1 i))
      )))
(mdefun reset-mem() ;Raffraichit la RAM !
  (let ((i 1))
    ;La case 0 m'appartient !
    (clrhash Table-liens)
    (loop while (<= i TailleMemoire) do
      (Ecris-Memoire i 0)
      (setf i (+ 1 i)))
    (setf Dernier-Code-Charge 1)
    )
  )

;Gauthier a fait une fonction équivalente avec un nom plus anglo-saxon !
(mdefun compile-charge (fct)
	(progn
	  (vm-compile-function fct)
	  (charge-code (get-compiled-function-body fct) fct)))
()