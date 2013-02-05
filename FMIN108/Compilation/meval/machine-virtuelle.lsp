;Lors du chargement d'une fonction, si on lis une variable globale
;et qu'elle n'a jamais été introduite celà veut dire qu'on accède à l'environnement
;clisp. Il faut donc se débrouiller à évaluer ce symbole et réserver une adresse mémoire
;pour ce symbole.





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

;IMPORTANT Laisser tomber les variables globales pour une table de hashage
;Garantissant un accés au registre par rapport à leur nom.

;(load "pile.lsp") ;Gestion de la pile, évaluation des symboles et registres !
;(load " utils_fonctions.lsp") ; Gestion du code compilé
;(setf sortie (open "tmp" :direction :output))
;
;(defvar A0 0)
;(defvar A1 0)
;(defvar A2 0)
;(defvar A3 0)
;(defvar A4 0)
;(defvar A5 0)
;(defvar SP -1)
;(defvar FP -1)
;(defvar PC -1)

;(load "env.lsp")

(mdefun recherche-adresse-dans-le-code (code adresse)
  ;Je suppose que l'adresse dans le code est un symbole qui n'est
  ;pas dans une sous-liste
;  (if (atom code) ;Si on n'a pas trouvé l'adresse
;      (list code nil)         ;On aura un arret de la machine virtuelle.
;    (if (equal (car code) adresse)
;	(car code)
;    (list code (recherche-adresse (cdr code) adresse))
;     )
;    )
  (list code (member adresse code))

  )
 

(mdefun load-code (adresse)
	(let ((adresse-code (get-compiled-function-body adresse :include-macros t)))
	  (list adresse-code adresse-code)))
 
  

(mdefun suiv ()
  (list (codepc (gethash 'PC registres)) (cdadr (gethash 'PC registres))))



(mdefun pointeur (pc)
  (cadr pc))

(mdefun codepc (pc)
  (car pc))



;FONCTIONS POUR ALLEGER LE CODE : S doit etre un registre de la meme forme que PC.  
(mdefun valeur-op1()
  (retourne-valeur (op1))
  )
(mdefun valeur-op2()
  (retourne-valeur(op2))
)

(mdefun valeur-op3()
  (retourne-valeur(op3))
  )

(mdefun op0()
  (caar (pointeur (gethash 'PC registres)))
)

(mdefun op1()
  (cadar (pointeur (gethash 'PC registres)))
  )
(mdefun op2()
  (caddar (pointeur (gethash 'PC registres)))
)

(mdefun op3()
  (car (cdddar (pointeur (gethash 'PC registres))))
)

(mdefun traitement-saut-op()
  (setf (gethash 'PC registres) (recherche-adresse-dans-le-code (codepc (gethash 'PC registres)) (op3)))
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
  (if (> nombre 0)
      (progn
	;(format *standard-output* "~W ~W" (elt stack depart))
	(cons (retourne-direct-pile  depart)
	      (recuperer-liste-parametres (- depart 1) (- nombre 1)))
	)
    nil))

(mdefun appel-fonction (fonction)
  
(let* ((mon-prototype (copy-tree (get-compiled-function-prototype fonction :include-macros t)))
	( parametres (nreverse (recuperer-liste-parametres (- (retourne-valeur 'fp) 1)
							   (retourne-pile 0))))
	 nouveaux-parametres
	 (sauvegarde-sommet (make-array 4))
	 (i 3)
	 (ancien-nb-param (length parametres))
	 nb-param 
	 )

;  (print "****************************")
;  (print mon-prototype)
;  (print parametres)
;  (print "****************************")

(if (null mon-prototype)
    nil
  (if (is-simple-prototype  mon-prototype)
      
      (if (= (length (get-prototype-variables mon-prototype)) ancien-nb-param)
	  (load-code fonction)
	(error "Mauvais parametrage !"))
    (progn
      (format *standard-output* "protoype : ~S~%" mon-prototype)
      (format *standard-output* "parametre : ~S~%" parametres)
    (if (assoc-prototype-parameters  mon-prototype parametres )
	 (progn
	;   (print "****************************")
	 ;  (print mon-prototype)
	  ; (print parametres)
	   ;(print "****************************")
	   
	   
	   (setf nouveaux-parametres (get-prototype-values mon-prototype)) ;creation des nouveaux parametres

	   (loop while (>= i 0) do                   ;sauvegarde des 4 premieres cases du sommet
	     (setf (elt sauvegarde-sommet i) (gpop))
	     (setf i (- i 1)))
	   
	   (affecte-registre 'sp (- (retourne-valeur 'sp) ancien-nb-param 1)) ;Comme si on dépilait !
	   
	   (setf nb-param (length nouveaux-parametres))
	   
	   (setf i 0)
	   (loop while (< i nb-param) do            ;empile les paramètres
	     (gpush (elt nouveaux-parametres i))
	     (setf i (+ 1 i)))
	   
	   (gpush nb-param)                       ;empile leur nombre
	   
	   (setf i 0)
	   (loop while (< i 4) do                 ;rempile le début du cadre
	     (gpush (elt sauvegarde-sommet i))
	     (setf i (+ 1 i))
	     )
	   (affecte-registre 'fp (+ (- nb-param ancien-nb-param) (retourne-valeur 'fp))) ; Ah que c'est beau !
	   (load-code fonction))                   ;Renvoie le code de la fonction
	 
	 (error "Mauvais parametrage !!")
	 )
     
       
       
       
      )
     )
    )
  )
)	       
	  






(mdefun traitement-fct-lisp ( )
	(progn
	  (format *standard-output* "Fonction  Lisp!~%")
	  
	  (let ((liste-parametres (nreverse (recuperer-liste-parametres (- (retourne-valeur 'fp) 1)
									(retourne-pile 0)))))
	    
	    (format *standard-output* "liste parametres : ~S~%" liste-parametres) 
	    (print `(,(op1) ,@liste-parametres))
	    ;(affecte-registre 'A0 (eval `(,(op1) ,@liste-parametres)))
	    (affecte-registre 'A0 (apply (op1) liste-parametres))
	    (affecte-registre 'fp (gpop))
	    (affecte-registre 'sp (gpop))
	    
	    )
	  )
	)


(mdefun vm-eval (code)
  ;code est la liste des instructions à évaluer
;               
;                                      code  pointeur
;                                       ^      ^
;                                       |      |
;                                       |      |
;                                       |      |
  (let ((avance t))        ;            |      |
    (progn  ;                           |      |       
      (reset) ;                         |      | 
      (affecte-registre 'pc `(,code ,code))
      (loop while (not (null (pointeur (gethash 'PC registres)))) do ; tant qu'on est sur une instruction
			
	(loop while (not (listp (car (pointeur (gethash 'PC registres))))) do
	    (affecte-registre 'pc (suiv))) ;Si c'est une étiquette passe !
	
	
	
	;#######################
	;# Debogueur pas à pas #
	;#######################
	(let ((chaine-lue "n'importe quoi!"))
	  (progn
	    (format *standard-output* "!! DEBUGGAGE PAS A PAS !! ~%")
	    (format *standard-output* "Instruction courante : ~S~%" (car (pointeur (gethash 'PC registres)))) 
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
		(affecte-registre 'pc (list (codepc (gethash 'pc registres)) nil)))
		;(affecte-registre 'pc (list (codepc (gethash 'pc registres)) nil)))
	       
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
	
	;#####################
	;# Machine Virtuelle #
	;#####################

	(case (op0)
	  (JMP (affecte-registre 'pc (recherche-adresse-dans-le-code (codepc (gethash 'PC registres)) (op1))))
	  (JSR
	   (let ((tmp nil))
	     ;(print (load-code (op1)))
	     (gpush (suiv)) ; sauve l'instruction suivante
	    ; (print "p1")
	     (setf tmp (recherche-adresse-dans-le-code (codepc(gethash 'PC registres)) (op1)))
	     
	     (if (null (pointeur tmp)) (setf tmp (appel-fonction (op1))))
	     ;(print "p2")
	     (if (null (pointeur tmp)) 
		 (progn 
		   (setf tmp (gpop))
		   ;(format *standard-output* "tmp : ~S~%" tmp)
		   
		   (traitement-fct-lisp))) 
	       
	     (affecte-registre 'pc tmp)
	     
	     )
	   )

	  ;;;;;;;;;;;;;OPERATEURS DE LISTES;;;;;;;;;;;;;;;;;;;;
	  (CONS 
	   (test-des-operateurs (op1) (op2))
	   (affecte-registre (op1)  (cons (gethash (op1) registres)  (gethash (op2) registres)))
	   (affecte-registre 'pc (suiv)))
	  
	  (CAR 
	   (if (not (est-registre (op1)))
	       (error "~S n'est pas un registre" (op1)))
	   (affecte-registre (op1)  (car (gethash (op1) registres)))
	   (affecte-registre 'pc (suiv)))
	  
	  (CDR
	   (if (not (est-registre (op1)))
	       (error "~S n'est pas un registre" (op1)))
	   (affecte-registre (op1)  (cdr (gethash (op1) registres)))
	   (affecte-registre 'pc (suiv)))

	  ; Ajout de Gauthier
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

	   ; Ajout de GAUTHIER car tu n'etais pas la et j'en avais besoin 
	   (NOT (affecte-registre (op1) (not (valeur-op1)))
		(affecte-registre 'pc (suiv)))
	   ; fin ajout
	   
	   (JF (if (null (gethash 'A0 registres))
		   (affecte-registre 'pc 
			 (recherche-adresse-dans-le-code 
			  (codepc (gethash 'PC registres)) 
			  (op1)
			  )
			 )
		 (affecte-registre 'pc (suiv))))
	   

	   (JT (if (not (null (gethash 'A0 registres)))
		   (affecte-registre 'pc (recherche-adresse-dans-le-code (codepc (gethash 'PC registres)) (op1)))
		 (affecte-registre 'pc (suiv))))
	   
		 
	  ;Retourne une variable de la pile dans un registre
	  (LOCAL (affecte-registre (op1) (retourne-pile (valeur-op2)))
		 (affecte-registre 'pc (suiv)))
	  ;affecte une variable de la pile
	  (SETLOCAL (affecte-pile (valeur-op1) (valeur-op2))
		   (affecte-registre 'PC (suiv) ))
	  ;Retourne une variable globale dans un registre
	  (GLOBAL 
	   (est-registre (op1))
	   (affecte-registre (op1) (valeur-op2))
	   (affecte-registre 'PC (suiv))) 
	  
	  ;Affecte une variable globale
	  ;ATTENTION ! SETGLOBAL peut introduire une NOUVELLE variable !!!!
	  ;AH QUE C'EST PAS BEAU !
	  (SETGLOBAL 
	   (est-registre (op2))
	   (setf (gethash (op1) variables-globales) (gethash (op2) registres))
	   (affecte-registre 'PC (suiv)))
	  
	  (PUSH
	   (est-registre (op1)) 
	   (gpush (gethash (op1) registres))
	   (affecte-registre 'pc (suiv))
	   )
	  (POP  
	   (est-registre (op1))
	   (affecte-registre (op1) (gpop))
	   
	   (if (not (eq 'pc (op1)))
	       (affecte-registre 'pc (suiv))) 
	   
	   )

	  ;MOVE fonctionne uniquement de registre à registre
	  ;Penser à faire une vérification
	  (MOVE 
	   (test-des-operateurs (op1) (op2))
	   (affecte-registre (op1) (gethash (op2) registres))
	   (affecte-registre 'pc (suiv)))

	  ;MOVEI fonctionne uniquement avec une variable immédiate 
	  (MOVEI 
	   (est-registre (op1))
	   (affecte-registre (op1) (op2))
	   (affecte-registre 'pc (suiv)))	
	  
	  (RTN (affecte-registre 'pc (gpop)))
	  (END (setf avance t)	 ; on passe en mode debuggage
	       (affecte-registre 'pc (suiv)))
	  
	  (ENDC (if (= (valeur-op1) (valeur-op2)) 
		    (affecte-registre 'pc (list (codepc (gethash 'pc registres)) nil))
		  (affecte-registre 'pc (suiv))))
	  
	  (t (error "Gauthier a toujours raison"))
	  )
	
		
	
	)
      
      (retourne-valeur 'A0)
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
  (let ((i 0))
    (if (= -1 (retourne-valeur 'SP))
	(format *standard-output* "Pile Vide ~%")

      (loop while (<= i (retourne-valeur 'SP)) do
	(format *standard-output* "| ~S |~%" (retourne-direct-pile i))
	(setf i (+ 1 i))
	)
      )
    )
  )

; exemple    
; (vm-eval '((move a0 5) T1 (add a0 a0) (jeq 40 a0 t2) (jmp T1) T2))
