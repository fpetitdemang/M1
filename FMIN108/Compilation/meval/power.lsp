
;##############################################
;##                                          ##
;##   Machine virtuelle                      ##
;##                                          ##
;##           Eric Boulat                    ##
;##                                          ##
;##                                          ##
;##                                          ##
;##                                          ##
;##############################################
;
;La fonction vm-eval fonctionne en itératif
;pour "coller" au fonctionnement d'un vrai processeur.
;Celà dit, étant donné la présence d'instructions de haut niveau
;de gestion des listes, on peut se demander si ces scrupules
;sont justifiés
;De toutes manières des versions futures de cette machine virtuelle
;colleront plus avec une machine réelle. Gestion d'une mémoire,
;stockage du code assembleur dans un tableau et gestion d'adresse
;mémoire par rapport à ce tableau.

;Pour l'instant les variables globales sont gérées dans une table de hashage

;Pour passer à l'instruction suivante
(defvar debogage t)

(mdefun debogage-active()
	(setf debogage t))

(mdefun debogage-desactive()
	(setf debogage nil))

(mdefun debogage-inverse()
	(setf debogage (not debogage)))

(mdefun debogage()
	debogage)


(mdefun suiv ()
  (+ (gethash 'pc registres) 1))


;FONCTIONS POUR ALLEGER LE CODE : S doit etre un registre de la meme forme que PC.  
(mdefun valeur-op1()
  (retourne-valeur (op1))
  )
(mdefun valeur-op2()
  (retourne-valeur(op2))
)

(mdefun valeur-op3()
  (retourne-valeur(op2)))

(mdefun op0()
  (car (Memoire (gethash 'PC registres)))
)

(mdefun op1()
  (cadr (Memoire (gethash 'PC registres)))
  )
(mdefun op2()
  (caddr (Memoire (gethash 'PC registres)))
)

(mdefun op3()
  (cadddr (Memoire (gethash 'PC registres)))
)

;Traitement des sauts conditionnels avec 2 opérateurs
(mdefun traitement-saut-op()
  (if (numberp (op3))
      (setf (gethash 'PC registres) (op3))
    (error "ADRESSE NON RESOLUE ~S~%" (op3)))
)

(mdefun est-registre (operande)
	(member operande liste-registres)
)



(mdefun test-des-operateurs ( operande1 operande2)
  (if (and (est-registre operande1) (est-registre operande2))
      t

    (error "~S ; ~S : Un de ces 2 symboles n'est pas un registres~%" operande1 operande2))
  )

(mdefun nombre-ou-registre (operande)
  ;Renvoie vrai si l'operande est un registre ou un nombre
  (or (numberp operande) (est-registre operande)))


(mdefun test-nb-ou-reg (operande)
  (if (not (nombre-ou-registre operande))
      (progn
	(format *standard-output* "~S n'est ni un nombre ni un registre~%" operande)
	(error "")
	)
    )
  )
      

(mdefun recuperer-liste-parametres(depart nombre) ;Attention la liste est à l'envers !
(if (> Depart TailleMemoire) nil 
  (if (> nombre 0)
      (progn
	
	(cons (Memoire depart)
	      (recuperer-liste-parametres (+ depart 1) (- nombre 1)))
	)
    nil)))

;Traitement d'un appel à une fonction CLisp
(mdefun traitement-fct-lisp ( )
	(progn
	 
	  
	  (let ((liste-parametres (nreverse (recuperer-liste-parametres (+ (retourne-valeur 'fp) 1) 
							      (retourne-pile 0)))))
	    
	   
	    (affecte-registre 'A0 (apply  (op1) liste-parametres))
	    (affecte-registre 'fp (mem-pop))
	    (affecte-registre 'sp (mem-pop))
	    
	    )
	  )
	)



(mdefun vm-eval (Debut &optional (debug t))
  ;Debut est l'adresse mémoire ou commence l'exécution
  
  (let ((avance t) (arret-total nil))                    
    (progn                                    
                              
      (affecte-registre 'pc Debut)
      (loop while (and (not (null (Memoire (gethash 'pc registres))))(not (eq (Memoire (gethash 'pc registres)) 0)) (not arret-total)) do ; tant qu'on est sur une instruction
	
	(if  (and debogage debug) 
	    
	
					;#######################
					;# Debogueur pas à pas #
					;#######################
		   (let ((chaine-lue "n'importe quoi!"))
		     (progn
		       (format *standard-output* "!! DEBUGGAGE PAS A PAS !! ~%")
		       (format *standard-output* "~S ====> Instruction courante : ~S~%" (gethash 'PC registres) (Memoire (gethash 'PC registres))) 
		       (loop while (and avance (string-not-equal chaine-lue "D")) do
			     (setf chaine-lue (string-upcase(read *standard-input*)))
			     (cond 
			      ((string-equal chaine-lue "P") (affpile))
			      ((string-equal chaine-lue "R") (reg))
	       
			      ((string-equal chaine-lue "RP") 
			       (reg)
			       (affpile))
			      
			      ((string-equal chaine-lue "PR") 
			       (affpile)
			       (reg))
			      ((string-equal chaine-lue "END") ;Provoque l'arret définitif du programme débuggé
			       (setf avance nil)
			       (setf arret-total t))
			      
			      
			      ((string-equal chaine-lue "CONT") (setf avance nil)) ; Arrete le débuggage et termine l'exécution
			      
			      
			      ((string-equal chaine-lue "D") (format *standard-output* "~%~%")) ; Debugge l'instruction suivante
			      ((string-equal chaine-lue "H") 
			       (format *standard-output* "P pour afficher la pile~%")
			       (format *standard-output* "R pour afficher les registres~%")
			       (format *standard-output* "END provoque l'arret définitif du programme debugge~%")
			       (format *standard-output* "CONT continue le programme sans debuggage~%")
			       (format *standard-output* "D passe à l'instruction suivante~%")
			       (format *standard-output* "H affiche cette aide~%"))
			      
	       
			      (t (format *standard-output* "Instruction debuggage inconnue ! ~S ~%" chaine-lue)))
			     )
		       )
		     )
		   
	  )
					;#####################
					;# Machine Virtuelle #
					;#####################
	
	(case (op0)
	  (JMP (if (numberp (op1))
		   (affecte-registre 'pc (op1))
		  (progn 
		    (setf tmp (mem-pop))
		    (traitement-fct-lisp) 
		    (affecte-registre 'pc tmp)))
	     
	     )
		  
	  (JSR
	   (let ((tmp nil))
	     
	     (mem-push (suiv)) ; sauve l'instruction suivante
			
	     
	     (if (numberp (op1)) ;Adresse resolue
		 (affecte-registre 'pc (op1))
	       
	          
	       (progn 
		 (setf tmp (mem-pop))
		 (traitement-fct-lisp) 
		 (affecte-registre 'pc tmp)))
	     
	     )
	   )
	  
	  ;;;;;;;;;;;;;OPERATEURS DE LISTES;;;;;;;;;;;;;;;;;;;;
	  (CONS 
	   (test-des-operateurs (op1) (op2))
	   (affecte-registre (op1)  (cons (gethash (op1) registres)  (gethash (op2) registres)))
	   (affecte-registre 'pc (suiv)))
	  
	  (CAR 
	   (test-des-operateurs (op1) (op1))
	   (affecte-registre (op1)  (car (gethash (op1) registres)))
	   (affecte-registre 'pc (suiv)))
	  
	  (CDR
	   (test-des-operateurs (op1) (op1))
	   (affecte-registre (op1)  (cdr (gethash (op1) registres)))
	   (affecte-registre 'pc (suiv)))

	
			    
	  (SETCAR
	   (test-des-operateurs (op1) (op2))
	   (setf (car (valeur-op1)) (valeur-op2))
	   (affecte-registre 'pc (suiv)))

	  (SETCDR
	   (test-des-operateurs (op1) (op2))
	   (setf (cdr (valeur-op1)) (valeur-op2))
	   (affecte-registre 'pc (suiv)))

	  (SETHASH
	   (test-des-operateurs (op1) (op2))
	    (if (not (est-registre (op3)))
	       (error "~S n'est pas un registre" (op3)))
	   (setf (gethash (valeur-op1) (valeur-op2)) (valeur-op3))
	   (affecte-registre 'pc (suiv)))

	  (SETELT
	   (test-des-operateurs (op1) (op2))
	   (if (not (est-registre (op3)))
	       (error "~S n'est pas un registre" (op3)))
	   (setf (elt (valeur-op1) (valeur-op2)) (valeur-op3))
	   (affecte-registre 'pc (suiv)))

	  ; fin ajout
	  



  
	      ;;;;;;;;OPERATEURS ARITHMETIQUES;;;;;;;;;;;;;;;
	  
	  (ADD
	   (est-registre (op1))
	   (test-nb-ou-reg (op2))
	   (affecte-registre (op1) (+ (gethash (op1) registres) (valeur-op2)))
	   (affecte-registre 'pc (suiv)))
	  
	  (SUB 
	   (est-registre (op1))
	   (test-nb-ou-reg (op2))
	   (affecte-registre (op1)  (- (gethash (op1) registres)  (valeur-op2)))
	   (affecte-registre 'pc (suiv)))

	  (MUL 
	   (est-registre (op1))
	   (test-nb-ou-reg (op2))
	   (affecte-registre (op1)  (* (gethash (op1) registres)  (valeur-op2)))
	   (affecte-registre 'pc (suiv)))

	  (DIV 
	   (est-registre (op1))
	   (test-nb-ou-reg (op2))
	   (affecte-registre (op1)  (/ (gethash (op1) registres) (valeur-op2)))
	   (affecte-registre 'pc (suiv)))


	  ;;;;;;;;;;Sauts avec comparaison;;;;;;;;;;;;;;;

	;  Premier opérande :Tout comme le deuxième ça peut etre un registre comme une valeur immédiate
	;                                       
	;                                       
	  (JEQ 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (equal (valeur-op1)  (valeur-op2))
	       (traitement-saut-op)
	     (affecte-registre 'pc (suiv))
	     )
	   )
	       ;(print (pointeur (gethash 'PC registres)))
	  (JL 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (< (valeur-op1)  (valeur-op2))
	       (traitement-saut-op)
	     (affecte-registre 'pc (suiv))
	     ))

	  (JLE 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (<= (valeur-op1)  (valeur-op2))
	       (traitement-saut-op)
	     (affecte-registre 'pc (suiv))
	     ))
	  (JGE 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (>= (valeur-op1)  (valeur-op2))
	       (traitement-saut-op)
	     (affecte-registre 'pc (suiv))
	     ))
	  
	  (JG 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	      (if (> (valeur-op1)  (valeur-op2))
		  (traitement-saut-op)
		(affecte-registre 'pc (suiv))
		)
	      )
	  
	  (GT 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (> (valeur-op1) (valeur-op2))
		  (affecte-registre 'A0 (valeur-op1))
		(affecte-registre 'A0 nil))
	   (affecte-registre 'pc (suiv)))
	  
	  (GE 
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (>= (valeur-op1) (valeur-op2))
		  (Affecte-registre 'A0 (valeur-op1))
		(affecte-registre 'A0 nil))
		(affecte-registre 'pc (suiv)))	
	  (LT
	   (test-nb-ou-reg (op1))
	   (test-nb-ou-reg (op2))
	   (if (< (valeur-op1) (valeur-op2))
		  (affecte-registre 'A0 (valeur-op1))
		(affecte-registre 'A0 nil))
		(affecte-registre 'pc (suiv)))

	   (LE 
	    (test-nb-ou-reg (op1))
	    (test-nb-ou-reg (op2))
	    (if (<= (valeur-op1) (valeur-op2))
		  (affecte-registre 'A0 (valeur-op1))
		(affecte-registre 'A0 nil))
		(affecte-registre 'pc (suiv)))
	   
	   (EQ
	    (test-nb-ou-reg (op1))
	    (test-nb-ou-reg (op2))
	    (if (= (valeur-op1) (valeur-op2))
		(affecte-registre 'A0 (valeur-op1))
	      (affecte-registre 'A0 nil))
	    (affecte-registre 'pc (suiv)))
	   
	   (JF (if (null (gethash 'A0 registres))
		   (affecte-registre 'pc (op1))
		 (affecte-registre 'pc (suiv))))
	   

	   (JT (if (not (null (gethash 'A0 registres)))
		   (affecte-registre 'pc (op1))
		 (affecte-registre 'pc (suiv))))
	   
	   (NOT 
	    (est-registre (op1))
	    (affecte-registre (op1) (not (valeur-op1)))
	    (affecte-registre 'pc (suiv)))
	   
	   
		 
	  ;Retourne une variable de la pile dans un registre
	  (LOCAL (affecte-registre (op1) (retourne-pile (- (valeur-op2))))
		 (affecte-registre 'pc (suiv)))
	  ;affecte une variable de la pile
	  (SETLOCAL (affecte-pile (- (valeur-op1)) (valeur-op2))
		   (affecte-registre 'PC (suiv) ))
	  ;Retourne une variable globale dans un registre
	  (GLOBAL 
	   (est-registre (op1))
	   (if (not (numberp (op2)))
	       (affecte-registre (op1) (eval (op2)))   ;La variable globale est dans l'environnement clisp
	     (affecte-registre (op1) (memoire (op2)))) ;Sinon elle est en RAM !
	   (affecte-registre 'PC (suiv))) 
	  
	  ;Affecte une variable globale
	  ;ATTENTION ! SETGLOBAL peut introduire une NOUVELLE variable !!!!
	  
	  (SETGLOBAL 
	   (est-registre (op2))
	   (if (not (numberp (op1)))
	       `(setf ,(op1) (gethash (op2) registres)) ;Affectation d'une variable globale de CLISP
	     (Ecris-memoire (op1) (valeur-op2)))
	   (affecte-registre 'PC (suiv)))
	  
	  (PUSH
	   (est-registre (op1)) 
	   (mem-push (gethash (op1) registres))
	   (affecte-registre 'pc (suiv))
	   )
	  (POP  
	   (est-registre (op1))
	   (affecte-registre (op1) (mem-pop))
	   
	   (if (not (eq 'pc (op1)))
	       (affecte-registre 'pc (suiv))) 
	   
	   )

	  ;MOVE fonctionne uniquement de registre à registre
	  ;Penser à faire une vérification
	  (MOVE 
	   (test-des-operateurs (op1) (op2))
	   (affecte-registre (op1) (gethash (op2) registres))
	   (affecte-registre 'pc (suiv)))

	  ;MOVEI fonctionne uniquement avec une valeur immédiate 
	  (MOVEI 
	   (est-registre (op1))
	   (affecte-registre (op1) (op2))
	   (affecte-registre 'pc (suiv)))	
	  
	  ;HOMMAGE!
	  (POKE   ;ADRESSE VALEUR ;Ecris Valeur à Adresse
	   ;Valeur doit etre un regristre
	   ;Adresse peut etre un registre ou une valeur immediate mais pas une variable
	   (est-registre (op2))
	   (Ecris-Memoire (valeur-op1) (valeur-op2))
	   (affecte-registre 'pc (suiv)))
	  
	  (PEEK ;REGISTRE ADRESSE
	   ;ADRESSE PEUT ETRE UN REGISTRE OU UNE VALEUR IMMEDIATE
	   (est-registre (op1))
	   (affecte-registre (op1) (Memoire (valeur-op2)))
	   (affecte-registre 'pc (suiv)))



	  (RTN (affecte-registre 'pc (mem-pop)))
	  (END (setf arret-total t))	 ; on arrete tout
	  
	  (ENDC (if (= (valeur-op1) (valeur-op2)) 
		    (progn
		     (debogage-active)
		     (affecte-registre 'pc (suiv)))
		  (affecte-registre 'pc (suiv))))
	  
	  (t (error "Instruction Inconnue"))
	  )
	
		
	
	)
      
      (print (retourne-valeur 'A0))
      )
    )
  )
  
(mdefun reg() ; Excellente fonction de trace des registres
  (format *standard-output* " A0 : ~S A1 : ~S A2 : ~S A3: ~S A4: ~S A5: ~S SP : ~S FP ~S ~%" 
	  (retourne-valeur 'A0) 
	  (retourne-valeur 'A1) 
	  (retourne-valeur 'A2)
	  (retourne-valeur 'A3)
	  (retourne-valeur 'A4)
	  (retourne-valeur 'A5)
	  (retourne-valeur 'SP)
	  (retourne-valeur 'FP)))


 
      
  

(mdefun affpile() ;fabuleuse fonction de trace de la pile
  (let ((i (- TailleMemoire 1)))
    (if (= TailleMemoire (retourne-valeur 'SP))
	(format *standard-output* "Pile Vide ~%")

      (loop while (>=  i (retourne-valeur 'SP)) do
	(format *standard-output* "~S:=========>| ~S |~%" i (Memoire i))
	(setf i (- i 1))
	)
      )
    )
  )


'(FIN-DU-VOYAGE) ;Marqueur de fin de fichier pour les chargeurs de 1ere génération. Les nouveaux n'en n'ont pas besoin !
()