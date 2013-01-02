;//---------------------------------------------------// 
;//---------------------------------------------------// 
;//       COMPILATEUR  -->  Tp Lisp 2007/2008         //
;//---------------------------------------------------// 
;//---------------------------------------------------// 


;//-----------------------------------------------
;// Compilation de l'expression passé en parametre 
;//-----------------------------------------------
(let ((liste ()))
(defun run_gc (expr &optional fichier)
  (setf liste ())
  ; Si l'expression est un atome alors nil
  (if (atom expr)
      ()
    (gc_asm expr () t ()))
  (gc_concat '(HALT))
    
  (if fichier
      (ecrire (reverse liste) fichier)
    (reverse liste)))


;//-----------------------------------
;// Genere le code ASM de l'expression
;//-----------------------------------
(defun gc_asm (expr env top_level envPre)

  (if (atom expr)
      (gc_val expr env envPre)
    (cond
   
 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


       
     ;----------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un QUOTE
     ;----------------------------------------------------------
     ((EQL 'QUOTE (car expr))
	 
      (if (listp (cadr expr))
	  (gc_val (cadr expr) env envPre)
	(gc_val (cadr expr) env envPre)))
	  
	



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     
     ;----------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un DEFUN
     ;----------------------------------------------------------

     ((EQ 'defun (car expr))

      ; On se trouve au debut du defun
      (gc_concat (list 'LABEL (cadr expr)))

      ; Creation de l'environnement en fonction des parametres
      (let ((env (make_param_env (caddr expr) env)))
	; On compile le corps de la fonction
	(gc_asm (cadddr expr) env () envPre)
	(gc_concat (list 'RTN))))



 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



     ;---------------------------------------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un operateur arithmétique ( + - * / )
     ;---------------------------------------------------------------------------------------

     ((operateur_arithmetique (car expr))

      ; Operation de type ( op X Y )

      ; On genere le code pour X 
      (gc_asm (cadr expr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))

      ; On genere le code pour Y
      (gc_asm (caddr expr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))

      (gc_concat (list 'POP 'R1))
      (gc_concat (list 'POP 'R0))

      ; On ajoute l'opération à réaliser
      (case (car expr )
	(+
	 (gc_concat (list 'ADD 'R1 'R0)))
	(-
	 (gc_concat (list 'SUB 'R1 'R0)))
	(*
	 (gc_concat (list 'MUL 'R1 'R0)))
	(/
	 (gc_concat (list 'DIV 'R1 'R0)))))



 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



     ;-------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un IF
     ;-------------------------------------------------------

     ((EQ 'if (car expr))

      ; Operation de type ( IF ( op X Y ) A B )
	
      (let ((saut1 (compteur++)))
	;(compteur++)
	(let ((saut2 (compteur++)))
	  ;(compteur++)

	  ; On s'occupe du Test
	  (gc_op (cadr expr) saut1 env top_level envPre)

	  ; Le if est faux alors :
          ; On compile le else --> B
	  (gc_asm (cadddr expr) env top_level envPre)

          ; On saute à la fin
	  (gc_concat (list 'JMP (list '@ saut2)))

	  ; Le if est vrai alors :
	  (gc_concat (list 'LABEL saut1))

          ; On compile le alors --> A
	  (gc_asm (caddr expr) env top_level envPre)

	  ; Etiquette de la fin du IF
	  (gc_concat (list 'LABEL saut2)))))
     


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	
     ;----------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un PROG1
     ;----------------------------------------------------------
     
     ((EQ 'prog1 (car expr))
      (let ((first (cadr expr)))
	(loop for elmt in (cddr expr) 
	      do
	      (gc_asm elmt env top_level envPre))
	(gc_asm first env top_level envPre)))


	
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



     ;----------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un PROGN
     ;----------------------------------------------------------
 
     ((EQ 'progn (car expr))	
      (loop for elmt in (cdr expr) 
	    do
	    (gc_asm elmt env top_level envPre)))



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



     ;---------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un COND
     ;---------------------------------------------------------

     ((EQ 'cond (car expr))
      ; (cond ((comp) vrai) ((comp) vrai))
      
      
      (let ((fin (compteur++)))
	;(compteur++)
	  
	(loop for val in (cdr expr)
	      do

	      (let ((saut1 (compteur++)))
		;(compteur++)
		(let ((saut2 (compteur++)))
		  ;(compteur++)
		  
                  ; On s'occupe du Test
		  (gc_op (car val) saut1 env top_level envPre)
			  
		  ; On saute à la fin
		  (gc_concat (list 'JMP (list '@ saut2)))
		  
	          ; Le if est vrai alors :
		  (gc_concat (list 'LABEL saut1))

                  ; On compile le alors --> A
		  (loop for elt in (cdr val) 
			do
			(gc_asm elt env top_level envPre))
		  (gc_concat (list 'JMP (list '@ fin)))

                  ; Etiquette de la fin d'un condition
		  (gc_concat (list 'LABEL saut2)))))	

	; Etiquette de la fin du IF
	(gc_concat (list 'LABEL fin))))
     


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     ;---------------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un LOOP WHILE
     ;---------------------------------------------------------------

     ((AND (EQ 'loop (car expr))
	   (EQ 'while (cadr expr))
	   (EQ 'do (cadddr expr)))

      (let ((boucle (compteur++)))
	;(compteur++)
	(let ((fin (compteur++)))
	  ;(compteur++)

	  ; Operation de type ( loop while ( op X Y ) A )

	  ; On se trouve a l'etiquette boucle
	  (gc_concat (list 'LABEL boucle))
		
          ; On s'occupe du Test
	  (gc_op (caddr expr) fin env top_level envPre)

          ; Sinon on compile l'expression A
	  (gc_asm (car (cddddr expr)) env top_level envPre)

	  ; On saut a l'etiquette boucle
	  (gc_concat (list 'JMP (list '@ boucle)))

	  ; On se trouve a l'etiquette fin
	  (gc_concat (list 'LABEL fin)))))      



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     ;---------------------------------------------------------------
     ; L'expression que l'on veut compiler correspond a un CAR ou CDR
     ;---------------------------------------------------------------

     ((or (EQ 'car (car expr))
	  (EQ 'cdr (car expr)))
      
      (gc_asm (cadr expr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))
      (gc_concat (list 'POP 'R1))
      (gc_concat (list (car expr) 'R1 'R0)))



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     ;-----------------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un LOOP FOR IN
     ;-----------------------------------------------------------------


     ((AND (EQ 'loop (car expr))
	   (EQ 'for (cadr expr))
	   (EQ 'in (cadddr expr))
	   (EQ 'do (cadr (cddddr expr))))
     
      (let ((etiq (compteur++)))
	;(compteur++)
	(let ((var (compteur++)))
	  ;(compteur++)
	  (gc_concat (list 'JMP (list '@ etiq)))
	  (gc_concat (list 'LABEL var))
	  (gc_concat '(NOP))
	  (gc_concat (list 'LABEL etiq))

	  (let ((etiq2 (compteur++)))
	    ;(compteur++)
	    (let ((var2 (compteur++)))
	      ;(compteur++)
	      (gc_concat (list 'JMP (list '@ etiq2)))
	      (gc_concat (list 'LABEL var2))
	      (gc_concat '(NOP))
	      (gc_concat (list 'LABEL etiq2))

	      (let ((env (gc_env '___LISTE___ (list '@ var) env)))
		(let ((env (gc_env (caddr expr) (list '@ var2) env)))

		  (let ((boucle (compteur++)))
		    ;(compteur++)
		    (let ((fin (compteur++)))
		      ;(compteur++)

		      (gc_asm (car (cddddr expr)) env top_level envPre)
		      (gc_concat (list 'STORE 'R0 (cadr (assoc '___LISTE___ env :test #'eql))))
		      (gc_concat (list 'LOAD (cadr (assoc '___LISTE___ env :test #'eql)) 'R2))
		      (gc_concat (list 'CAR 'R2 'R2))
		      (gc_concat (list 'STORE 'R2 (cadr (assoc (caddr expr) env :test #'eql))))

		      (gc_concat (list 'LABEL boucle))
		      
		      (gc_concat (list 'LOAD (cadr (assoc '___LISTE___ env :test #'eql)) 'R2))
		      (gc_concat (list 'MOVE (list '$ 'nil) 'R0))
		      
		      (gc_concat (list 'CMP 'R2 'R0))
		      (gc_concat (list 'JEQ (list '@ fin)))

		      (gc_asm (caddr (cddddr expr)) env top_level envPre)
		      		      
		      (gc_concat (list 'LOAD (cadr (assoc '___LISTE___ env :test #'eql)) 'R2))
		      (gc_concat (list 'CDR 'R2 'R2))
		      (gc_concat (list 'STORE 'R2 (cadr (assoc '___LISTE___ env :test #'eql))))
		      (gc_concat (list 'CAR 'R2 'R2))
		      (gc_concat (list 'STORE 'R2 (cadr (assoc (caddr expr) env :test #'eql))))
		      
		      (gc_concat (list 'JMP (list '@ boucle)))
		      (gc_concat (list 'LABEL fin)))))))))))





;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     ;---------------------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un LOOP FOR FROM TO
     ;---------------------------------------------------------------------


     ((AND (EQ 'loop (car expr))
	   (EQ 'for (cadr expr))
	   (EQ 'from (cadddr expr))
	   (EQ 'to (cadr (cddddr expr)))
	   (EQ 'do (cadddr (cddddr expr))))

      (let ((boucle (compteur++)))
	;(compteur++)
	(let ((fin (compteur++)))
	  ;(compteur++)

          ; Operation de type ( loop for X from Y to Z  W)

          ; On compile l'expr init --> Y
	  (gc_asm (car (cddddr expr)) env top_level envPre)

          ; On copie le resultat dans X
 	  (gc_concat (list 'MOVE 'RO (list '@ (caddr expr))))

          ; Si Z est un atome et un entier il n'est pas utile de la recalculer à chaque boucle
	  (if (atom (cadddr (cdddr expr)))

	      ; On compile l'expr fin --> Z
	      (progn 
		(gc_asm (car (cddddr (cddddr expr))) env top_level envPre)
		(gc_concat (list 'PUSH 'R0))

	        ; On se trouve sur l'etiquette de la boucle
		(gc_concat (list 'LABEL boucle))
		(gc_concat (list 'MOVE (list '@ (caddr expr)) 'R0))
		(gc_concat (list 'POP 'R1)))

	    ; sinon on recalcule Z a chaque tour de boucle
	    (progn
	      (gc_concat (list 'LABEL boucle))
	      (gc_asm (car (cddddr (cddddr expr))) env top_level envPre)
	      (gc_concat (list 'MOVE 'R0 'R1))
	      (gc_concat (list 'MOVE (list '@ (caddr expr)) 'R0))))
	  
	  (gc_concat (list 'SUB 'R0 'R1))
	  (gc_concat (list 'CMP 'R1 '($ 0)))
	  (gc_concat (list 'JLE (list '@ fin)))
          
          ; On compile l'expression W
	  (gc_asm (cadddr (cddddr expr)) env top_level envPre)
	  (gc_concat (list 'INCR (list '@ (caddr expr))))
	  
          ; On saute a la boucle
	  (gc_concat (list 'JMP (list '@ boucle)))
		
          ; On se trouve à la fin
	  (gc_concat (list 'LABEL fin)))))



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     ;---------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un LET
     ;---------------------------------------------------------

     ((EQ 'let (car expr))
      ; Il faut distingué deux cas
      ; 1 -> C'est un let au top level
      (if top_level
	  (let ((env (make_let_top_env (cadr expr) env)))
	    (gc_let (cddr expr) env top_level envPre))
        ; 2 -> C'est un let interne 
        ; On cree un environnement contenant toute les variables du let
	(let ((env (make_let_env (cadr expr) env (- 3 (length env)) top_level envPre)))
	  (gc_let (cddr expr) env top_level envPre)))
      (loop for elmt from 0 to (- (length (cadr expr)) 1)
	    do
	    (gc_concat (list 'DECR 'SP))))
	



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


       
     ;---------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un SETF
     ;---------------------------------------------------------
     ((EQ 'setf (car expr))
      ; (setf A X)
      
      ; On compile le X
      (gc_asm (caddr expr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))

      ; Ensuite on analyse A
      ; Cas ou A se trouve dans l'environnement
      (if (cadr (assoc (cadr expr) env :test #'eql))
	  (progn
	    (gc_concat (list 'POP 'R0))
	    (gc_concat (list 'STORE 'R0 (cadr (assoc (cadr expr) env :test #'eql)))))
	(progn
	  ; On genere le code pour Y de (FUNC X Y)
	  (gc_asm (car(cddadr expr)) env top_level envPre )
	  (gc_concat (list 'PUSH 'R0))
	
          ; On genere le code pour X de (FUNC X Y)
	  (gc_asm (cadadr expr) env top_level envPre)
	  (gc_concat (list 'PUSH 'R0))
	  (gc_concat (list 'POP 'R2))
	  (gc_concat (list 'POP 'R1))
	  (gc_concat (list 'POP 'R0))
	  (cond
	   ((EQL (caadr expr) 'get)
	    (gc_concat (list 'SETFGET 'R2 'R1 'R0)))
	   ((EQL (caadr expr) 'aref)
	    (gc_concat (list 'SETFAREF 'R2 'R1 'R0)))
	   ((EQL (caadr expr) 'gethash)
	    (gc_concat (list 'SETFHASH 'R2 'R1 'R0)))   
	   ((EQL (caadr expr) 'car)
	    (gc_concat (list 'SETFCAR 'R2 'R1 'R0)))    
	   ((EQL (caadr expr) 'cdr)
	    (gc_concat (list 'SETFCDR 'R2 'R1 'R0)))))))
 


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


       
     ;---------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un CASE
     ;---------------------------------------------------------
     ((EQ 'case (car expr))
      (let ((fin (compteur++)))
	;(compteur++)
	(loop for elmt in (cddr expr)
	      do
	      (let ((saut1 (compteur++)))
		;(compteur++)
		(let ((saut2 (compteur++)))
		  ;(compteur++)
		  (if (EQL (car elmt) 'otherwise)
		      (progn
			(gc_asm (cadr elmt) env top_level envPre)
			(gc_concat (list 'JMP (list '@ fin))))
		    (progn
                      ; On genere le code pour X 
		      (gc_asm (cadr expr) env top_level envPre)
		      (gc_concat (list 'PUSH 'R0))
	
                      ; On genere le code pour Y
		      (gc_asm (car elmt) env top_level envPre)
		      (gc_concat (list 'PUSH 'R0))

                      ; On recupere les elements X Y en memoire
		      (gc_concat (list 'POP 'R1))
		      (gc_concat (list 'POP 'R0))

                      ; Comparaison entre R0 et R1	
		      (gc_concat (list 'CMP 'R0 'R1))
		      (gc_concat (list 'JNEQ (list '@ saut2)))

                      ; On compile le vrai --> A
		      (gc_asm (cadr elmt) env top_level envPre)
		      (gc_concat (list 'JMP (list '@ fin)))

	              ; Etiquette de la fin du IF
		      (gc_concat (list 'LABEL saut2)))))))
	(gc_concat (list 'LABEL fin))))
     
	    


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


       
     ;-----------------------------------------------------------
     ; L'expression que l'on veut compiler correspond à un LABELS
     ;-----------------------------------------------------------
     ; (labels ((name (para) body) (...)) (name .. ..))
     ((EQ 'labels (car expr))
      (let ((env_locale (make_labels_env (cadr expr) (compteur++) ())) (etiq_fin (compteur++)))
	; Sauvegarde du precedent FP
	(gc_concat (list 'MOVE 'FP 'R0))
	(gc_concat (list 'PUSH 'R0))

	(gc_asm (caddr expr) env_locale top_level env)
	  
	; On stop le Label
	(gc_concat (list 'DECR 'SP))
	(gc_concat (list 'JMP (list '@ etiq_fin)))

	(loop for locale in (cadr expr)
	      do
	      (gc_concat (list 'LABEL (cadr (assoc (car locale) env_locale :test #'eql))))
	      (gc_concat (list 'MOVE 'FP 'R0))
	      (gc_concat (list 'PUSH 'R0))
	      (let ((env_locale (make_param_env (cadr locale) env_locale)))
		(gc_asm (caddr locale) env_locale top_level env))
	      (gc_concat (list 'DECR 'SP))
	      (gc_concat (list 'RTN)))
	
	(gc_concat (list 'LABEL etiq_fin))))

	 



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      ;----------------------------------------------------------------------------------
      ; L'expression que l'on veut compiler ne correspond à aucune des fonctions definies
      ;----------------------------------------------------------------------------------

     (t
      (if (atom (car expr))
	  (progn
	    
	    (gc_par (cdr expr) env top_level envPre )
	      
	    ;bloc A
	    (gc_concat (list 'MOVE (list '$ (length (cdr expr))) 'R0))
	    (gc_concat (list 'PUSH 'R0))
	    (gc_concat (list 'INCR 'R0))
	    (gc_concat (list 'MOVE 'FP 'R1))
	    (gc_concat (list 'MOVE 'SP 'FP))
	    (gc_concat (list 'MOVE 'SP 'R2))
	    (gc_concat (list 'SUB 'R0 'R2))
	    (gc_concat (list 'PUSH 'R2))
	    (gc_concat (list 'PUSH 'R1))

            ; On recupere dans etiq l'etiquette de expr dans l'environnement si elle existe
	    (let ((etiq (cadr (assoc (car expr) env :test #'eql))))              
              ; Si etiq est null on n'est pas dans un label
	      (if (null etiq)
		  (gc_concat (list 'JSR (list '@ (car expr))))
		(gc_concat (list 'JSR (list '@ etiq)))))

            ;bloc B
	    (gc_concat (list 'POP 'R1))
	    (gc_concat (list 'POP 'R2))
	    (gc_concat (list 'MOVE 'R1 'FP))
	    (gc_concat (list 'MOVE 'R2 'SP)))
	(error "Erreur: ~S ne peut pas etre compiler." (car expr)))))))



;//---------------------------------------------------------------------
;// Realise la concatenation du code ASM et ajoute le tout dans la liste
;//---------------------------------------------------------------------
(defun gc_concat (code_asm)
   (setf liste (cons code_asm liste))))

;//-------------------------------------------
;// Compile les expressions passer dans le let
;//-------------------------------------------
(defun gc_let (lexpr env top_level envPre)
  (if (atom lexpr)
      ()
    (progn
      (gc_asm (car lexpr) env top_level envPre)
      (gc_let (cdr lexpr) env top_level envPre))))



;//--------------------------------------------
;// Compile les expressions passer en parametre
;//--------------------------------------------
(defun gc_par (lexpr env top_level envPre) 
  (if (atom lexpr)
      ()
    (progn
      (gc_asm (car lexpr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))
      (gc_par (cdr lexpr) env top_level envPre))))


;//-------------------------------------------------------------------------------------------------------------------------------
;// Compile une expression atomique de forme entier ou symbol si il est connu dans l'environnement ou l'environnement pre (LABELS)
;//-------------------------------------------------------------------------------------------------------------------------------
(defun gc_val (var env envPre)
  ; On regarde si la var est un symbole
  (if (AND
       (symbolp var)
       (NOT (EQL var 'nil)))
              
      ; On recupere dans res la valeur de var dans l'environnement si elle existe
      (let ((res (cadr (assoc var env :test #'eql))) (res2 (cadr (assoc var envPre :test #'eql))))
      	; Si res est null il n'a pas été definie precedemment
	(if (null res)
	    (if (null res2)
		(gc_concat (list 'MOVE (list '$ var) 'R0))
	      (progn
		(gc_concat (list 'MOVE 'FP 'R2))
		(gc_concat (list 'MOVE (list 3 'FP) 'FP))
		(gc_concat (list 'MOVE res2 'R0))
		(gc_concat (list 'MOVE 'R2 'FP)) 
		))

	  (if (atom res)
	      (gc_concat (list 'MOVE res 'R0))
	    (if (EQ (car res) '@)
		(gc_concat (list 'LOAD res 'R0))
	      (gc_concat (list 'MOVE res 'R0))))))
	
    (gc_concat (list 'MOVE (list '$ var) 'R0))))



;//------------------------------------------------------------------------
;// Realise la compilation d'un forme (op X Y) se trouvant comme Test de IF
;//------------------------------------------------------------------------
(defun gc_op (expr etiq env top_level envPre)
  (cond
   ((EQL 't expr)
    (gc_concat (list 'JMP (list '@ etiq))))

   ((atom expr)
    (gc_asm expr env top_level envPre)
    (gc_concat (list 'CMP 'R0))
    (gc_concat (list 'JEQ (list '@ etiq))))
   
   ((operateur_comparaison (car expr))
    (gc_op_part expr env top_level envPre)
    (gc_comp expr etiq))

   ((EQL (car expr) 'AND)
    (gc_and (cdr expr) (compteur++) etiq (compteur++) env top_level envPre))

   ((EQL (car expr) 'OR)
    (gc_or (cdr expr) etiq env top_level envPre))

   ((EQL (car expr) 'NOT)
    (gc_not (cdr expr) etiq env top_level envPre))

   (t
    (gc_asm expr env top_level envPre)
    (gc_concat (list 'CMP 'R0))
    (gc_concat (list 'JEQ (list '@ etiq))))))

(defun gc_and (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (gc_concat (list 'LABEL etiq))
    (gc_concat (list 'JMP (list '@ etiqFin)))
    (gc_concat (list 'LABEL etiqFaux)))
   ((operateur_comparaison (caar expr))
    (gc_concat (list 'LABEL etiq))
    (gc_op_part (car expr) env top_level envPre)
    (gc_comp (car expr) (compteur++))      
    (gc_concat (list 'JMP (list '@ etiqFaux)))
    (gc_and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   ((EQ 'NOT (caar expr))
    (gc_not (cdar expr) (compteur++) env top_level envPre)
    (gc_concat (list 'JMP (list '@ etiqFaux)))
    (gc_and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (gc_concat (list 'LABEL etiq))
    (gc_asm (car expr) env top_level envPre)
    (gc_concat (list 'CMP 'R0))
    (gc_concat (list 'JEQ (list '@ (compteur++))))
    (gc_concat (list 'JMP (list '@ etiqFaux)))
    (gc_and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))))

(defun gc_not_and (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (gc_concat (list 'LABEL etiq))
    (gc_concat (list 'JMP (list '@ etiqFin)))
    (gc_concat (list 'LABEL etiqFaux)))
   ((operateur_comparaison (caar expr))
    (gc_concat (list 'LABEL etiq))
    (gc_op_part (car expr) env top_level envPre)
    (gc_not_comp (car expr) (compteur++))      
    (gc_concat (list 'JMP (list '@ etiqFaux)))
    (gc_not_and (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (gc_concat (list 'LABEL etiq))
    (gc_asm (car expr) env top_level envPre)
    (gc_concat (list 'CMP 'R0))
    (gc_concat (list 'JNEQ (list '@ (compteur++))))
    (gc_concat (list 'JMP (list '@ etiqFaux)))
    (gc_not_and (cdr expr) etiqFaux etiqFin (compteur) env top_level))))

(defun gc_or (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((operateur_comparaison (caar expr))
    (gc_op_part (car expr) env top_level envPre)
    (gc_comp (car expr) etiq) 
    (gc_or (cdr expr) etiq env top_level envPre))
   ((EQ 'NOT (caar expr))
    (gc_not (cdar expr) etiq env top_level envPre)
    (gc_or (cdr expr) etiq env top_level envPre))
   (t
    (gc_asm (car expr) env top_level envPre)
    (gc_concat (list 'CMP (caar expr) 'R0))
    (gc_concat (list 'JEQ (list '@ etiq)))
    (gc_or (cdr expr) etiq  env top_level envPre))))

(defun gc_not_or (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((operateur_comparaison (caar expr))
    (gc_op_part (car expr) env top_level envPre)
    (gc_not_comp (car expr) etiq) 
    (gc_not_or (cdr expr) etiq env top_level envPre))
   (t
    (gc_asm (car expr) env top_level envPre)
    (gc_concat (list 'CMP (caar expr) 'R0))
    (gc_concat (list 'JNEQ (list '@ etiq)))
    (gc_not_or (cdr expr) etiq  env top_level envPre))))

(defun gc_not (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((operateur_comparaison (caar expr))
    (gc_op_part (car expr) env top_level envPre)
    (gc_not_comp (car expr) etiq) 
    (gc_not (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'AND)
    (gc_not_and (cdar expr) (compteur++) etiq (compteur++) env top_level envPre)
    (gc_not (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'OR)
    (gc_not_or (cdar expr) etiq env top_level envPre)
    (gc_not (cdr expr) etiq env top_level envPre))

   (t
    (gc_asm (car expr) env top_level envPre)
    (gc_concat (list 'CMP (caar expr) 'R0))
    (gc_concat (list 'JNEQ (list '@ etiq)))
    (gc_not (cdr expr) etiq  env top_level envPre))))

(defun gc_op_part (expr env top_level envPre)
  (progn
    (gc_asm (cadr expr) env top_level envPre)
    (gc_concat (list 'PUSH 'R0))
    (gc_asm (caddr expr) env top_level envPre)
    (gc_concat (list 'PUSH 'R0))
    (gc_concat (list 'POP 'R1))
    (gc_concat (list 'POP 'R0))
    (gc_concat (list 'CMP 'R0 'R1))))

(defun gc_comp (expr etiq)
  (case (car expr)
    (=
     (gc_concat (list 'JEQ (list '@ etiq))))
    (EQ
     (gc_concat (list 'JEQ (list '@ etiq))))
    (EQL
     (gc_concat (list 'JEQ (list '@ etiq))))
    (EQUAL
     (gc_concat (list 'JEQ (list '@ etiq))))
    (<
     (gc_concat (list 'JL (list '@ etiq))))
    (>
     (gc_concat (list 'JG (list '@ etiq))))
    (!=
     (gc_concat (list 'JNEQ (list '@ etiq))))
    (<=
     (gc_concat (list 'JLE (list '@ etiq))))
    (>=
     (gc_concat (list 'JGE (list '@ etiq))))))

(defun gc_not_comp (expr etiq)
  (case (car expr)
    (=
     (gc_concat (list 'JNEQ (list '@ etiq))))
    (EQ
     (gc_concat (list 'JNEQ (list '@ etiq))))
    (EQL
     (gc_concat (list 'JNEQ (list '@ etiq))))
    (EQUAL
     (gc_concat (list 'JNEQ (list '@ etiq))))
    (<
     (gc_concat (list 'JGE (list '@ etiq))))
    (>
     (gc_concat (list 'JLE (list '@ etiq))))
    (!=
     (gc_concat (list 'JEQ (list '@ etiq))))
    (<=
     (gc_concat (list 'JG (list '@ etiq))))
    (>=
     (gc_concat (list 'JL (list '@ etiq))))))


;//----------------------------------
;// Creer l'environnement pour un let
;//----------------------------------
(defun make_let_env (lexpr env acc top_level envPre)
  (if (atom lexpr)
      env
    (progn
      (gc_asm (cadar lexpr) env top_level envPre)
      (gc_concat (list 'PUSH 'R0))
      (make_let_env (cdr lexpr) (gc_env (caar lexpr) (list (+ (length env) acc) 'FP) env) acc top_level envPre ))))



;//-----------------------------------------------
;// Creer l'environnement pour un let au top level
;//-----------------------------------------------
(defun make_let_top_env (lexpr env)
  (if (atom lexpr)
      env
    (let ((etiq (compteur++)))
      ;(compteur++)
      (let ((var (compteur++)))
	;(compteur++)
	(gc_concat (list 'JMP (list '@ etiq)))
	(gc_concat (list 'LABEL var))
	(gc_concat (list (cadar lexpr)))
	(gc_concat (list 'LABEL etiq))
	(make_let_top_env (cdr lexpr) (gc_env (caar lexpr) (list '@ var) env))))))


;//-------------------------------------
;// Creer l'environnement pour un labels
;//-------------------------------------
(defun make_labels_env (lexpr etiq env)
  (if (atom lexpr)
      env
    (make_labels_env (cdr lexpr) (compteur++) (gc_env (caar lexpr) etiq env))))


;//----------------------------------------------
;// Creer l'environnement a partir des parametres
;//----------------------------------------------
(defun make_param_env (lpara env)
  (if (atom lpara)
      env
    (make_param_env (cdr lpara) (gc_env (car lpara) (list (- 0 (+ (length lpara) 1)) 'FP) env) )))


;//------------------------------------------------------------
;// Ajoute à l'environnement le nouveau couple (symbole valeur)
;//------------------------------------------------------------
(defun gc_env (symbole valeur env)
  (acons symbole (list valeur) env))

 

;//-----------------------------------------------------------
;// Retourne vrai si l'operateur appartient à la liste définie
;//-----------------------------------------------------------
(defun operateur_arithmetique ( op )
  (member op '(+ - * /)))



;//-----------------------------------------------------------
;// Retourne vrai si l'operateur appartient à la liste définie
;//-----------------------------------------------------------
(defun operateur_comparaison ( op )
  (member op '(= < > != <= >= EQ EQL EQUAL)))



;//----------------------------------------------------------------
;// Permet d'ecrire le code ASM ligne par ligne dans un fichier ASM
;//----------------------------------------------------------------
(defun ecrire (listeASM path)
  (with-open-file (stream path :direction :output)
		  (loop for elmt in listeASM
			do
			(format stream (toString elmt)))
		  (format stream "nil")))

(defun toString (a)
  (format nil "~A~%" a))



;//----------------------------------------------------
;// Permet de charger un fichier lisp et de le compiler
;//----------------------------------------------------

(defun gcload (file &optional fichier)
  (let ((res ()))
    (labels ((lecture (o)
		      (let ((lu (read o nil nil nil)))
			(if lu
			    (progn 
			      (setf res (append (run_gc lu) res))
			      (lecture o))
			  't))))
      (lecture (open file)))
    (if fichier
	(ecrire res fichier)
      res)))
    


;//-----------------------
;// Creation d'un compteur
;//-----------------------
(let ((etiq 0)) (defun compteur () etiq) (defun compteur++ () (setf etiq (+ etiq 1))) (defun reset () (setf etiq 0)))

