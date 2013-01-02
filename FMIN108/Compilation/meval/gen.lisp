; COMPILATION DU CODE LISP
;
; Le compilateur est capable de gerer des imbrications quelconques de 'let', 'let*', 'labels'.
; Il gere les macros de la facon suivante : lorsqu'il rencontre une macro compilee, il l'expanse via
; la MACHINE VIRTUELLE puis compile le resultat; lorsqu'il rencontre une macro LISP, il l'expanse via
; 'macroexpand-1' puis compile le resultat. Si une macro est a la fois compilee et definie dans LISP,
; le compilateur choisit la difficulte et expanse la macro compilee.
; Bien qu'il ne gere pas les closures et les valeurs fonctionnelles, le compilateur est neanmoins
; assez complet pour pouvoir compiler du code LISP assez courant. Il peut, par exemple, compiler
; le META-EVALUATEUR, LA MACHINE VIRTUELLE et lui-meme entierement.
; Neanmoins, celui-ci fait quelques hypotheses 'contraignantes' pour la generation des appels :
;	- si dans une fonction compilee 'foo' il y a un appel a une fonction 'goo' qui n'est ni
; compilee, ni mdefinie au moment de la compilation de l'appel et qui a '&optional', &key' ou &rest' dans sa
; liste de parametres, mais qui compilee et chargee dans la MACHINE VIRTUELLE au moment de
; l'execution de l'appel, les valeurs des variables optionnelles, par mots-cles ou du reste
; sont relativement quelconques
;
;	-  si dans une fonction compilee 'foo' il y a un appel a une fonction 'goo' qui est mdefinie
; ou compilee au moment de la compilation de l'appel et qui a des variables par mots-cles ou un reste
; dans sa liste de parametres, et que celle-ci n'est pas compilee et chargee dans la MACHINE VIRTUELLE
; au moment de l'execution de l'appel, l'appel de fonction qui va etre redirige vers LISP par la
; machine virtuelle risque de provoquer des erreurs car les mots-cles auront ete supprimes par le
; compilateur et celui-ci aura deja construit un argument correspondant au reste, ce qui devrait dans
; ce cas etre fait par LISP
;
; Bonne utilisation du compilateur :
;	- mdefinissez uniquement les fonctions que vous voulez compiler
;	- compilez-les
;	- chargez-les dans la machine virtuelle
;	- mdefinissez les fonctions restantes


; expr : une expression LISP

; resultat : renvoie le code compile correspondant a 'expr'

(mdefun vm-compile(expr)
  (compile-main expr (empty-env) (make-status))
  )


; expr : une expression LISP
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile correspondant a 'expr' dans l'environnement 'env'
; et avec un statut 'statut'

(mdefun compile-main(expr env status)
	;(print expr)
  ; l'expression est une liste
  (if (consp expr)
      (cond
       ; l'expression est un appel de fonction ou de macro
       ((symbolp (car expr))
	(cond
		; appel a une fonction compilable directement (defun, setf, progn,...)
	 	((is-builtin-function (car expr))
		  	(compile-builtin-function expr env status))

	         ; appel a une macro compilee
		 ((get-compiled-function (car expr) :macros-only t)
		 	(displace expr (expand-compiled-macro expr env status))
			(compile-main expr env status))

	         ; appel a une macro qui n'est pas compilee
		 ((macro-function (car expr))
		  	(displace expr (macroexpand-1 expr))
	       		(compile-main expr env status))

	         ; appel a une combinaison de car et cdr (cadr, cddr,... mais pas car ou cdr)
		 ((is-car-cdr-combination (car expr))
		  	(displace expr (simplify-car-cdr-combination expr))
			(compile-main expr env status))

		 ; sinon, appel a une fonction 'classique'
		 (t (compile-call expr env status))))

	 ; l'expression est l' application d'une lambda-expression
	 ((and ;(consp (car expr))
	       (is-lambda-expression (car expr)))
	       ;(eq (caar expr) 'lambda))
	  		(compile-lambda expr env status))

	 (t		(error "compile-main: ~W n'est pas une fonction, ni une lambda-expression (recu: ~W)" (car expr) expr)))


    ; l'expression est une variable
    (if (is-variable expr)
	(compile-get-variable expr env status)

      ; sinon, l'expression est une constante
      (compile-constant expr env status)))
  )


; expr : une liste dont le premier element est un nom de fonction geree directement par le compilateur
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

; rem : cette fonction ne fait qu'appeler la fonction de compilation dediee a la fonction integree

(mdefun compile-builtin-function(expr env status)
  (let ((name (car expr)))
    (case name

	  (apply	(compile-apply expr env status))
	  (quote 	(compile-quote expr env status))
	  (function	 (compile-function-quote expr env status))

	  ; on compile 'defun' et 'defmacro' sans tenir compte de l'environnement englobant
	  (defun 	(compile-function expr nil (make-status :base 0 :current-env nil :terminal-recursion t)))
	  (defmacro 	(compile-function expr nil (make-status :base 0 :current-env nil :terminal-recursion t)))

	  (defparameter (compile-defparameter expr env status))
	  (labels 	(compile-labels expr env status))
	  ((setf setq)	(compile-setf expr env status))
	  (progn 	(compile-progn expr env status))
	  (let		(compile-let expr env status))
	  (let*		(compile-let* expr env status))
	  (if		(compile-if expr env status))
	  (while	(compile-while expr env status))
	  (loop		(compile-loop expr env status))
	  (for		(compile-for expr env status))
	  (cons		(compile-cons expr env status))
	  (car		(compile-car expr env status))
	  (cdr		(compile-cdr expr env status))
	  (list		(compile-list expr env status))
	  (list*	(compile-list* expr env status))
	  (+		(compile-add expr env status))
	  (1+		(compile-increment expr env status))
	  (-		(compile-sub expr env status))	
	  (1-		(compile-decrement expr env status))
	  (*		(compile-mul expr env status))
	  (/		(compile-div expr env status))
	  ((= < <= > >=)(compile-number-comparison expr env status))
	  (not		(compile-not expr env status))

	  (t		(warn "compile-builtin-function: ~W n'est pas une fonction integree (recu : ~W)" (car expr) expr)
			nil)))
  )


; expr : une liste dont le premier element est 'apply'
; env : une environnement de compilation
; status : un statut de compilation

; resultat : le code compile de 'expr' suivant 'env' et 'status'

(mdefun compile-apply(expr env status)
  (labels
   ; expr : une expression quelconque

   ; resultat : renvoie vrai si 'expr' une lambda-expression #-cotee, faux sinon
   ((is-quote-lambda-expression(expr)
			       (and (consp expr)
				    (eq (car expr) 'function)
				    (= (length expr) 2)
				    (is-lambda-expression (cadr expr)))
			       )
    
    
    ; expr : une expression quelconque

    ; resultat : renvoie vrai si 'expr' est un symbole cote, faux sinon
    (is-quote-symbol-expression(expr)
			       (and (consp expr)
				    (eq (car expr) 'quote)
				    (= (length expr) 2)
				    (symbolp (cadr expr))))


    ; expr : une expression quelconque

    ; resultat : renvoie vrai si 'expr' est un symbole #-cote, faux sinon
    (is-function-symbol-expression(expr)
				  (and (consp expr)
				       (eq (car expr) 'function)
				       (= (length expr) 2)
				       (symbolp (cadr expr)))))
    
    (cond
     ; pas suffisamment d'arguments pour 'apply'
     ((< (length expr) 3)
      		(error "compile-apply: trop peu d'arguments (recu: ~W)" expr))

     ; le premier argument est une lambda-expression
     ((is-lambda-expression (cadr expr))
      		(compile-main `(,(cadr expr)  ,(list*-to-cons `(list* ,@(cddr expr)))) env status))

     ; le premier argument est une lambda-expression #-cotee
     ((is-quote-lambda-expression (cadr expr))
      		(compile-main `(,(cadadr expr)  ,(list*-to-cons `(list* ,@(cddr expr)))) env status))

     ; le premier argument est un symbole cote ou #-cote
     ((or (is-function-symbol-expression (cadr expr))
	  (is-quote-symbol-expression (cadr expr)))
      		(compile-main `(,(cadadr expr) ,(list*-to-cons `(list* ,@(cddr expr)))) env status))

     ; sinon, on ne sait pas gerer directement, donc on genere un appel a 'apply'
     (t 	(compile-call expr env status))))
     ;(t (compile-call `(apply ,(cadr expr) ,(list*-to-cons `(list* ,@(cddr expr)))) env status))))
  )


; expr : une liste dont le premier element est 'quote'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-quote(expr env status)
  (case (length (cdr expr))
    	; expression de la forme '(quote)'
	(0	(error "compile-quote: trop peu d'arguments (recu : ~W)" expr))

	; expression de la forme '(quote <expr>)'
	(1	`((MOVEI A0 ,(cadr expr))))
	
	; sinon, on ne connait pas
	(t	(error "compile-quote: trop d'arguments (recu : ~W)" expr)))
  )


; expr : une liste dont le premier element est 'function'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : le code compile de 'expr' selon 'env' et 'status'

; rem : les valeurs fonctionnelles d'un symbole sont ramenees a un symbol, ce qui ne marche que
; lorsqu'il s'agit de la valeur fonctionnelle d'une fonction LISP, et que celle-ci est utilisee avec 'apply'.
; Les valeurs fonctionnelles des lambda-expressions ne sont pas gerees

(mdefun compile-function-quote(expr env status)
  (let ((length (length expr)))
    (cond
     ; expression de la forme '(function)'
     ((= length 1)
       		(error "compile-function-quote: trop peu d'arguments (recu : ~W)" expr))

     ; plus d'un argument pour 'function'
     ((> length 2)
      		(error "compile-function-quote: trop d'arguments (recu : ~W)" expr))

     ((is-lambda-expression (cadr expr))
      		(error "compile-function-quote: expression non geree (recu : ~W)" expr))

     ((symbolp (cadr expr))
      		`((MOVEI A0 ,(cadr expr))))

     (t		(error "compile-function-quote: l'argument n'est ni une lambda-expression, ni un nom de fonction (recu : ~W)" expr))))
  )


; expr : une liste dont le premier element est 'defun' ou 'defmacro'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie nil et enregistre la fonction ou macro compilee dans l'environnement du META-EVALUATEUR

(mdefun compile-function(expr env status)
  (cond
   ; pas assez de parametres pour 'defun' ou 'defmacro'
   ((< (length expr) 3)
    		(error "compile-function: trop peu d'arguments (recu : ~W)" expr))
   
   ; le 1er parametre n'est pas un nom de fonction i.e. n'est pas un symbole
   ((not (symbolp (cadr expr)))
    		(error "compile-function: ~W n'est pas un nom de fonction valide (recu : ~W)" (cadr expr) expr))
   
   ; sinon, l'expression est bien formee
   (t (let ; nom de la fonction
	  ((name (cadr expr))
	   ; on construit son prototype a partir de sa liste de parametres
	   (prototype (make-function-prototype (caddr expr)))
	   ; on recupere le corps du 'defun' ou 'defmacro'; si celui-ci est vide, on le transforme
	   ; alors en '(list nil)' afin qui le compilateur genere une instruction qui renvoie nil
	   (body (or (cdddr expr)
		     (list nil)))
	   ; vrai si on compile un 'defmacro', faux si on compile un 'defun'
	   (is-macro (eq (car expr) 'defmacro)))
	
	; la construction du prototype a reussi i.e. la liste des parametres est valide
	(if prototype
		   ; on ajoute la fonction ou la macro a l'environnement des fonctions,
		   ; et on cree un nouveau niveau dans l'environnement des variables pour les
		   ; parametres de la fonction
	    (let* ((new-env (add-param-variables (add-one-function env `(,name ,prototype) name :is-macro is-macro)
						 (get-prototype-variables prototype)))

		   ; on cree un statut de compilation correspondant au corps du 'defun' ou 'defmacro';
		   ; 'base' est mis a 0, 'in-label' a nil et 'terminal-recursion' a t car on est
		   ; dans un nouveau cadre de pile
		   (new-status (make-status :base 0
					    :current-env (function-env new-env)
					    :in-label nil
					    :terminal-recursion t)))

	      ; on recupere le code compile de la fonction
	      (setf body `(; etiquette qui indique le debut de la fonction, qui est son nom
			   ,name
			   ; on compile le corps de la fonction
			   ,@(multi-compile body new-env new-status)

			   ; on efface le bloc de pile d'appel, et on restitue
			   ; l'environnement de l'appelant
			   (POP A1)	; A1 <- valeur retour
			   (POP A2)	; A2 <- ancien FP
			   (POP SP)	; SP <- ancien SP => le bloc d'appel est efface
			   (MOVE FP A2)	; on restaure FP
			   (PUSH A1)	; on replace la valeur de retour sur la pile
			   (RTN)	; on retourne a la fonction appelante
			   )))

	  ; la liste des parametres est invalide
	  (error "compile-function: prototype incorrect (recu : ~W)" expr))
	
	; on enregistre la fonction ou la macro dans l'environnement du META-EVALUATEUR
	(if is-macro
	    (out "*** enregistrement de la macro ~W dans l'environnement du META-EVALUATEUR ***~%" name)
	  (out "*** enregistrement de la fonction ~W dans l'environnement du META-EVALUATEUR ***~%" name))
	
	(add-compiled-function name prototype body :is-macro is-macro)

	; on renvoie nil
	nil)))
  )


; expr : une liste dont le premier element est 'defparameter'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

; rem : le compilateur ne genere pas de declaration de variables globales; c'est la MACHINE VIRTUELLE
; qui se charge de reconnaitre les variables globales qu'il connait deja, et celles qu'il rencontre
; pour la premiere fois, lorsqu'il rencontre les instructions GLOBAL et SETGLOBAL

(mdefun compile-defparameter(expr env status)
  (let ((expr-length (length expr))
	; on interdit la recursivite terminale
	(new-status (make-status :base (get-base status)
				 :current-env (get-current-env status)
				 :in-label (is-in-label status)
				 :terminal-recursion nil)))
    (cond
     ((= expr-length 1)
      		(error "compile-defparameter: trop peu d'arguments (recu : ~W)" expr))

     ((> expr-length 3)
      		(error "compile-defparameter: trop d'arguments (recu : ~W)" expr))
     
     ((is-variable (cadr expr))
      		(if (= expr-length 2)
		    `((SETGLOBAL ,(cadr expr) nil)
		      (MOVEI A0 nil))
				  
		  `(,@(compile-main (caddr expr) env new-status)
		      (SETGLOBAL ,(cadr expr) A0))))
     
     (t		(error "compile-defparameter: ~W n'est pas une variable (recu : ~W)" (cadr expr) expr))))
  )


; expr : une liste dont le premier element est 'labels'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-labels(expr env status)
  (labels
   ; expr : une expression quelconque

   ; resultat : renvoie vrai si 'expr' est une definition de fonction i.e.
   ; de la forme '(name parameters expr1.. exprn)' et renvoie alors la declaration correspondante i.e.
   ; le couple '(name prototype)' ou 'prototype' est le prototype correspondant a 'parameters',
   ; la liste d'arguments de la fonction; sinon, renvoie nil
   ((is-function-definition(expr)
			   (and (listp expr)
				(>= (length expr) 3)
				(symbolp (car expr))
				(let ((prototype (make-function-prototype (cadr expr))))
				  (if prototype
				      (list (car expr) prototype)
				    nil))))

    
    ; lexpr : une liste d'expressions quelconques
    ; lfunc : une liste de declarations de fonctions (initialement nil)

    ; resultat : renvoie la liste de declarations de fonctions correspondant a 'lexpr' si 'lexpr'
    ; est une liste de definitions de fonctions, nil sinon
    (is-function-definition-list(lexpr lfunc)
				(if lexpr
				    (let ((func (is-function-definition (car lexpr))))
				      (if func
					  (is-function-definition-list (cdr lexpr) (cons func lfunc))
					
					nil))
				  (nreverse lfunc)))


    ; func : une declaration de fonction i.e. un couple '(name prototype)'
    ; body : le corps de la fonction
    ; env : un environnement de compilation
    ; status : un statut de compilation

    ; resultat : renvoie le code correspondant a la fonction selon 'env' et 'statut'
    (compile-one-function(func body env status)
			       ; on cree un nouveau niveau dans l'environnement des variables pour
			       ; les parametres de la fonction
			 (let ((new-env (add-param-variables env (get-prototype-variables (cadr func)))))

			   `(; on recupere dans l'environnement le symbole
			     ; de saut associe a la fonction
			     ,(get-function-symbol env (car func))

			     ; on compile le corps dans l'environnement englobant
			     ; augmente des parametres de la fonction
			     ,@(multi-compile  body new-env status)

			     ; on efface le bloc de pile de l'appelant, et on restaure le
			     ; l'environnement de l'appelant
			     (POP A1)		; A1 <- valeur retour
			     (POP A2)		; A2 <- ancien FP
			     (POP SP)		; SP <- ancien SP => le bloc de pile d'appel est efface
			     (MOVE FP A2)	; on restaure FP
			     (PUSH A1)		; on replace la valeur de retour sur la pile
			     (RTN))))		; on retourne a la fonction appelante


    ; genere le code d'une liste de definitions de fonctions
    ; lfunc : une liste de declarations de fonctions
    ; lfuncdef : une liste de definitions de fonctions compatible avec 'lfunc' i.e. qui contient
    ; les memes fonctions et dans le meme ordre
    ; env : un environnement de compilation
    ; status : un statut de compilation

    ; resultat : renvoie le code compile de toutes les fonctions dans l'ordre dans lequel
    ; elles sont declarees (ou definies)
    (compile-functions(lfunc lfuncdef env status)
		      (if lfuncdef
			  `(,@(compile-one-function (car lfunc) (cddar lfuncdef) env status)
			      ,@(compile-functions (cdr lfunc) (cdr lfuncdef) env status))
			nil)))


   ; suffisamment d'arguments pour 'labels' et le premier argument est une liste
   (if (and (> (length expr) 2)
	    (consp (cadr expr)))

       (let (; on recupere la liste de declarations de fonctions correspondant au
	     ; deuxieme argument si celui-ci est bien une liste de definitions de
	     ; fonctions, nil sinon
	     (lfunc (is-function-definition-list (cadr expr) nil)))

	 ; la liste de definitions des fonctions est incorrecte
	 (if (null lfunc)
	     (error "compile-labels: la liste des functions est incorrecte (recu : ~W)" expr))

	 (let* (; environnement pour le corps des fonctions du 'labels'; chaque fonction
		; doit connaitre toutes les autres fonctions du 'labels'
		(new-env (if (is-in-label status)
				  (add-current-functions env lfunc)
				(add-functions env lfunc)))

	        ; symbole utilise par le compilateur pour reperer le debut du corps du 'labels'
	        ; qui est situe juste apres le code des fonctions du 'labels'
		(debut (gentemp))
	       
	        ; statut de compilation pour le corps du 'labels'; on indique qu'on est dans le
		; corps d'un 'labels'
	        (label-status (set-in-label (copy-status status) t))

	        ; statut de compilation pour le corps des fonctions; on place 'base' a 0, 'in-label'
	        ; a nil et 'terminal-recursion' a t car on est dans un nouveau cadre de pile
	        (function-status (make-status :base 0
					      :current-env (function-env new-env)
					      :in-label nil
					      :terminal-recursion t)))

	 ; on saute au debut du corps du 'labels'
	 `((JMP ,debut)

	   ; on genere le code compile des fonctions locales
	   ,@(compile-functions lfunc
				(cadr expr)
				new-env
				function-status)
	      
	   ; debut du corps du 'labels'
	   ,debut

	   ; on genere le code du corps du 'labels'
	   ,@(multi-compile (cddr expr) new-env label-status))))

	 ; le corps du 'labels' est vide ou la liste de definitions de fonctions est invalide
	 (error "compile-labels: la liste des functions est incorrecte ou le corps du labels est vide (recu : ~W)" expr)))
  )


; expr : une liste dont le premier element est 'setf'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-setf(expr env status)
  (let (; on cree un statut qui interdit la recursion terminale
	(non-terminal-status (copy-status status :change t :terminal-recursion nil)))

    (labels
     ; lexpr : une liste d'expressions quelconques (initialement, 'expr' sans 'setf')

     ; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'
     ((compile-setf-aux(lexpr)
		       (case (length lexpr)
			     ; pas d'argument donne a 'setf'
			     (0 nil)
			    
			     ; on a une place pour l'affectation mais pas de valeur a affecter
			     (1 (error "compile-setf-aux: nombre impair d'arguments (recu : ~W)" expr))
			    
			     ; sinon, on genere le code pour affecter le 2eme element au 1er,
			     ; et on recommence sur 'lexpr' sans les 2 1ers elements
			     (t ; on expanse le 1er element
			        
			      	(if (consp (car lexpr))
				    (displace (car lexpr)
					      (expand-macro (car lexpr) env non-terminal-status)))

				; on remplace les combinaisons de 'car' et de 'cdr'
				(if (and (consp (car lexpr))
					 (is-car-cdr-combination (caar lexpr)))
				    (displace (car lexpr) (simplify-car-cdr-combination (car lexpr))))
				(append
				 (cond
				  ; on affecte une variable
				  ((is-variable (car lexpr))
				   		`(,@(compile-main (cadr lexpr) env non-terminal-status)
						    ,@(compile-set-variable (car lexpr) env non-terminal-status)))

				 ; on affecte une constante
				 ((not (listp (car lexpr)))
				  		(error "compile-setf-aux: ~W n'est pas affectable (recu : ~W)" (car lexpr) expr))

				 ; affectation d'une expression a 2 elements
				 ((= (length (car lexpr)) 2)
				  (case (caar lexpr)
					; affectation du 'car' d'une liste
				    	(car `(; on genere le code de l'expression a affecter
					       ,@(compile-main (cadr lexpr) env non-terminal-status)
						 (PUSH A0)
						 ; on genere le code de l'expression dont
						 ; on affecte le 'car'
						 ,@(compile-main (cadar lexpr)
								 env
								 (make-status ; on prend en compte la valeur a
								  	      ; affecter mise sur la pile
								  	      :base (+ (get-base status) 1)
									      :current-env (get-current-env status)
									      :in-label (is-in-label status)
									      :terminal-recursion nil))
						 (POP A1)
						 ; on affecte A1 au 'car' de la liste dans A0
						 (SETCAR A0 A1)
						 (MOVE A0 A1)
						 ))

					; affectation du 'cdr' d'une liste
				  	(cdr `(; on genere le code de l'expression a affecter
					       ,@(compile-main (cadr lexpr) env non-terminal-status)
						 (PUSH A0)
						 ; on genere le code de l'expression dont
						 ; on affecte le 'cdr'
						 ,@(compile-main (cadar lexpr)
								 env
								 (make-status ; on prend en compte la valeur a
								  	      ; affecter mise sur la pile
								  	      :base (+ (get-base status) 1)
									      :current-env (get-current-env status)
									      :in-label (is-in-label status)
									      :terminal-recursion nil))
					    
						 (POP A1)
						 ; on affecte A1 au 'cdr' de la liste dans A0
						 (SETCDR A0 A1)
						 (MOVE A0 A1)
						 ))

					; sinon, on ne sait pas gerer
					(t	(error "compile-setf-aux: affectation inconnue ou incorrecte (recu : ~W)" expr))))

				 ; affectation d'une expression a 3 elements
				 ((= (length (car lexpr)) 3)
				  (case (caar lexpr)
				    ; affectation d'un element d'une table de hashage
				    (gethash `(; on genere le code de l'expression a affecter
					       ,@(compile-main (cadr lexpr) env non-terminal-status)
						 (PUSH A0)
						 ; on genere le code de la cle 
						 ,@(compile-main (cadar lexpr)
								 env
								 (make-status ; on prend en compte la valeur a
								  	      ; affecter mise sur la pile
								  	      :base (+ (get-base status) 1)
									      :current-env (get-current-env status)
									      :in-label (is-in-label status)
									      :terminal-recursion nil))
						 (PUSH A0)
						 ; on genere le code de la table dont on veut
						 ; modifier un element
						 ,@(compile-main (caddar lexpr)
								 env
								 (make-status ; on prend en compte la valeur a
								  	      ; affecter et la cle mises sur la pile
								  	      :base (+ (get-base status) 2)
									      :current-env (get-current-env status)
									      :in-label (is-in-label status)
									      :terminal-recursion nil))
						 
						 (POP A1)		; A1 <- cle
						 (POP A2)		; A2 <- valeur a affecter
						 (SETHASH A1 A0 A2)	; affecte A2 a la cle A1 dans la table A0
						 (MOVE A0 A2)		; A0 recoit la valeur de l'affectation
						 ))

				    ; affectation d'un element d'une sequence
				    (elt `(; on genere le code de l'expression a affecter
					   ,@(compile-main (cadr lexpr) env non-terminal-status)
					     (PUSH A0)
					     ; on genere le code de la sequence
					     ,@(compile-main (cadar lexpr)
							     env
							     (make-status ; on prend en compte la valeur a
									  ; affecter mise sur la pile
							      		  :base (+ (get-base status) 1)
							      		  :current-env (get-current-env status)
							      		  :in-label (is-in-label status)
							      		  :terminal-recursion nil))
					     (PUSH A0)
					     ; on genere le code de l'indice de l'element a affecter
					     ,@(compile-main (caddar lexpr)
							     env
							     (make-status ; on prend en compte la valeur a
								  	  ; affecter et la sequence mises sur la pile
							      		  :base (+ (get-base status) 2)
									  :current-env (get-current-env status)
									  :in-label (is-in-label status)
									  :terminal-recursion nil))
						 
					     (POP A1)		; A1 <- sequence
					     (POP A2)		; A2 <- valeur a affecter
					     (SETELT A1 A0 A2)	; affecte A2 a l'element de position A0
					     			; dans la sequence A1
					     (MOVE A0 A2)	; A0 recoit la valeur de l'affectation
					     ))

				    ; sinon, on ne sait pas gerer
				    (t		(error "compile-setf-aux: affectation inconnue ou incorrecte (recu : ~W)" expr))))

				 ; sinon, on ne sait pas traiter
				 (t  (error "compile-setf-aux: affectation inconnue ou incorrecte (recu : ~W)" expr)))

				 ; on compile les affectations restantes
				 (compile-setf-aux (cddr lexpr)))))))
  
     ; pas d'argument donne a 'setf'
     (if (null (cdr expr))
	 `(MOVEI A0 nil)
  
       ; sinon, traite 'expr' sans 'setf'
       (compile-setf-aux (cdr expr)))))
  )


; expr : une liste dont le premier element est 'progn'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-progn(expr env status)
  (if (cdr expr)
      (multi-compile (cdr expr) env status)

    `(MOVEI A0 nil))
  )


; expr : une liste dont le premier element est 'let'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-let(expr env status)
  (labels
   ; expr : une expression quelconque

   ; resultat : renvoie vrai si 'expr' est de la forme '((var1 val1) (var2) var3..)',
   ; faux sinon
   ((check-variable-list(expr)
			(cond
			 ((consp expr)
			   	(and (or (is-variable (car expr))
					 (is-variable-value (car expr)))
				     (check-variable-list (cdr expr))))

			 ((null expr)
			  	t)

			 (t	nil)))

   ; lval : une liste d'expressions qui correspondent aux valeurs des variables
   ; status : un statut de compilation

   ; resultat : renvoie le code compile qui construit l'environnement des variables locales sur la pile
   (compile-variables(lval status)
		      (if lval
			  `(; on compile la premiere valeur
			    ,@(compile-main (car lval) env status)
			      ; on l'empile
			      (PUSH A0)
			      ; on fait de meme pour les autres valeurs
			      ,@(compile-variables (cdr lval)
						   (make-status :base (+ (get-base status) 1)
								:current-env (get-current-env status)
								:in-label (is-in-label status)
								:terminal-recursion nil)))
			nil))


    ; resultat : renvoie le code qui efface l'environnement des variables locales de la pile
    (delete-variables()
		     (let ((var-count (length (cadr expr))))
		       (if (is-special-let)
			 ; on prend en compte le pseudo bloc d'appel qu'on a genere
			   `((ADD SP ,(+ var-count 5)))
			  
			 `((ADD SP ,var-count)))))


    ; resultat : renvoie le code compile pour generer un pseudo bloc d'appel dans la pile si il est
    ; necessaire, nil sinon. Ce bloc permet de conserver les memes valeurs d'indice pour acceder
    ; aux variables locales et aux parametres que l'on soit dans une fonction ou non
    (generate-virtual-frame()
			   ; si on n'est pas dans la portee d'un bloc d'appel i.e. si on n'est
			   ; ni dans une fonction , ni dans un 'let', 'let*' ou 'lambda'
			   (if (is-special-let)
			       `((MOVEI A0 0)
				 (PUSH A0)	; pas d'argument
				 (MOVE FP SP)	; on place correctement FP
				 (MOVEI A0 nil)
				 (PUSH A0)	; pas d'environnement englobant
				 (MOVE A0 SP)
				 (ADD A0 2)
				 (PUSH A0)	; SP avant debut empilement
				 (MOVEI A0 nil)
				 (PUSH A0)	; ancien FP quelconque puisque pas de bloc d'appel englobant
				 (PUSH A0)	; valeur de retour quelconque
				 )))

   
    ; resultat : renvoie vrai si on n'est pas dans la portee d'un bloc d'appel i.e. si on n'est
    ; ni dans une fonction , ni dans un 'let', 'let*' ou 'lambda'; renvoie faux sinon
    (is-special-let()
		   (and ; on n'est pas dans une fonction
		    	(null (get-current-env status))
			; on n'est pas dans un 'let', 'let*' ou 'lambda'
			(null (variable-env env)))))

   ; suffisamment d'arguments pour 'let' et le premier argument est un
   ; environnement de variables valide
   (if (and (>= (length expr) 3)
	    (check-variable-list (cadr expr)))
       
       (let (; on recupere la liste des variables
	     (vars (get-variables (cadr expr)))
	     ; on recupere la liste des valeurs associees aux variables
	     (values (get-values (cadr expr))))
	 `(; on genere eventuellement un pseudo bloc d'appel
	   ,@(generate-virtual-frame)
	   ; on compile les variables locales
	   ,@(compile-variables values
				(make-status :base (get-base status)
					     :current-env (get-current-env status)
					     :in-label (is-in-label status)
					     :terminal-recursion nil))
	   ; on compile le corps du 'let'
	   ,@(if (is-special-let)
		 ; si on a genere un pseudo bloc d'appel
		 (multi-compile (cddr expr)
				(add-local-variables env vars 0)
				(make-status :base (length vars)
					     :current-env (get-current-env status)
					     :in-label nil
					     :terminal-recursion nil))
	       
	       ; sinon
	       (multi-compile  (cddr expr)
			       (add-current-local-variables env vars (get-base status))
			       (make-status :base (+ (get-base status) (length vars))
					    :current-env (get-current-env status)
					    :in-label nil
					    :terminal-recursion (is-terminal-recursion status))))

	   ; on genere le code pour nettoyer la pile des variables locales et eventuellement
	   ; du pseudo bloc d'appel
	   ,@(delete-variables)))
 
     ; sinon
     (error "compile-let: liste de (variables valeurs) incorrecte ou trop peu d'arguments (recu : ~W)" expr)))
  )


; expr : une liste dont le premier element est 'let*'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-let*(expr env status)
  (labels
    ; lvar : une liste de la forme '((var1 val1) (var2) var3..)' non vide (initialement,
    ; le premier argument de 'let*')

   ; resultat : renvoie une expression 'let' equivalente a 'expr'
   ((convert-let*-to-let(lvar)
		      (if (cdr lvar)
			  `(let (,(car lvar)) ,(convert-let*-to-let (cdr lvar)))
			`(let ,lvar ,@(cddr expr))))

    ; expr : une expression quelconque

    ; resultat : renvoie vrai si 'expr' est de la forme '((var1 val1) (var2) var3..)',
    ; faux sinon
    (check-variable-list(expr)
			(cond
			 ((consp expr)
			  		(and (or (is-variable (car expr))
						 (is-variable-value (car expr)))
					     (check-variable-list (cdr expr))))

			 ((null expr)
			  		t)

			 (t		nil))))

  (cond
   ; pas assez d'arguments pour 'let*'
   ((< (length expr) 3)
    		(error "compile-let*: trop peu d'arguments (recu : ~W)" expr))

   ; pas de variable
   ((null (cadr expr))
    		`(,@(compile-main (caddr expr) env status)))

   ; l'environnement des variables est non vide et correct
   ((check-variable-list (cadr expr))
    		(compile-main (convert-let*-to-let (cadr expr)) env status))

   ; sinon
   (t		(error "compile-let*: ~W n'est pas une liste de (variables valeurs) (recu : ~W)" (cadr expr) expr))))
  )


; expr : une liste dont le premier element est 'if'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-if(expr env status)
  (case (length expr)
	; pas assez d'arguments pour 'if'
	((1 2)
	 	(error "compile-if: trop peu d'arguments (recu : ~W)" expr))
	
	; on a un 'if .. then ..'
	(3
	 	(let (; etiquette de saut apres le 'if'
		      (label-fin (gentemp))
		      ; statut interdisant la recursivite terminale
		      (non-terminal-status (copy-status status :change t :terminal-recursion nil)))
		  
		  `(; on genere le code de test
		    ,@(compile-main (cadr expr) env non-terminal-status)
		      ; si le test est faux, on saute apres le 'if'
		      (JF ,label-fin)
		      ; on genere le code du 'then'
		      ,@(compile-main (caddr expr) env status)
		      ; etiquette indiquant la fin du 'if'
		      ,label-fin)))

	; on a un 'if .. then .. else ..'
	(4
	 	(let (; etiquette de saut au 'else'
		      (label-sinon (gentemp))
		      ; etiquette de saut apres le 'if'
		      (label-fin (gentemp))
		      ; statut interdisant la recursivite terminale
		      (non-terminal-status (copy-status status :change t :terminal-recursion nil)))
		  
		  `(; on genere le code de test
		    ,@(compile-main (cadr expr) env non-terminal-status)
		      ; si le test est faux, on saute au 'else'
		      (JF ,label-sinon)
		      ; on genere le code du 'then'
		      ,@(compile-main (caddr expr) env status)
		      ; on saute apres le 'if'
		      (JMP ,label-fin)
		      ; debut du 'else'
		      ,label-sinon
		      ; on genere le code du 'else'
		      ,@(compile-main (cadddr expr) env status)
		      ; fin du 'if'
		      ,label-fin)))

	; sinon, on ne sait pas gerer
	(t	(error "compile-if: trop d'arguments (recu : ~W)" expr)))
  )


; expr : une liste dont le premier element est 'while'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-while(expr env status)
  (case (length expr)
	; pas assez d'arguments pour 'while'
	((1 2)
	 	(error "compile-while: trop peu d'arguments (recu : ~W)" expr))

	; sinon
	(t (let (; etiquette indiquant le debut du 'while'
		 (label-boucle (gentemp))
		 ; etiquette indiquant la fin de la boucle
		 (label-fin (gentemp))
		 ; statut interdisant la recursivite terminale
		 (non-terminal-status (copy-status status :change t :terminal-recursion nil)))

	     `(; debut de la boucle
	       ,label-boucle
	       ; on genere le code du test
	       ,@(compile-main (cadr expr) env non-terminal-status)
	       ; si le test est faux, on saute apres la boucle
	       (JF ,label-fin)
	       ; on genere le code de la boucle
	       ,@(multi-compile (cddr expr) env non-terminal-status)
	       ; on saute au debut de la boucle
	       (JMP ,label-boucle)
	       ; fin de la boucle
	       ,label-fin))))
  )


; expr : une liste dont le premier element est 'loop'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-loop(expr env status)
  (cond
   ; pas assez d'arguments pour 'loop'
   ((< (length expr) 4)
    		(error "compile-loop: trop peu d'arguments (recu : ~W)" expr))

   ; on traite le 'while .. do ..'
   ((and (eq (cadr expr) 'while)
	 (eq (cadddr expr) 'do))
    		(let (; etiquette indiquant le debut de la boucle
		      (label-boucle (gentemp))
		      ; etiquette indiquant la fin de la boucle
		      (label-fin (gentemp))
		      ; statut interdisant la recursivite terminale
		      (non-terminal-status (copy-status status :change t :terminal-recursion nil)))

		  `(; debut de la boucle
		    ,label-boucle
		    ; on genere le code du test
		    ,@(compile-main (caddr expr) env non-terminal-status)
		    ; si le test est faux, on saute apres la boucle
		    (JF ,label-fin)
		    ; on genere le code de la boucle
		    ,@(multi-compile (cddddr expr) env non-terminal-status)
		    ; on saute au debut de la boucle
		    (JMP ,label-boucle)
		    ; fin de la boucle
		    ,label-fin)))

   ; on ne traite pas les 'loop .. until ..' ou 'loop .. for ..'
   (t		(error "compile-loop: erreur de syntaxe ou forme non geree (recu : ~W)" expr)))
  )


; expr : une liste dont le premier element est 'for'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-for(expr env status)
  (cond
   ; pas assez d'arguments pour 'for'
   ((< (length expr) 3)
	 	(error "compile-for: trop peu d'arguments (recu : ~W)" expr))

   ; le deuxieme argument n'est pas une liste ou n'a pas le bon nombre
   ; d'arguments
   ((or (not (listp (cadr expr)))
	(not (= (length (cadr expr)) 3))
	(not (is-variable (caadr expr))))
    		(error "compile-for: deuxieme argument incorrect; attendu (variable valeur_init valeur_fin) (recu : ~W)" expr))

   ; sinon
   (t		(let (; etiquette indiquant le debut de la boucle
		      (label-boucle (gentemp))
		      ; etiquette indiquant la fin de la boucle
		      (label-fin (gentemp))
		      ; statut interdisant la recursivite terminale
		      (non-terminal-status (copy-status status :change t :terminal-recursion nil))
		      ; statut prenant en compte l'empilement d'un element sur la pile
		      (new-status (make-status :base (+ (get-base status) 1)
					       :current-env (get-current-env status)
					       :in-label (is-in-label status)
					       :terminal-recursion nil)))
		  
		  `(; on genere le code de la valeur initiale
		    ,@(compile-main (cadadr expr) env non-terminal-status)
		    ; on affecte la variable de la valeur initiale placee dans A0
		    ,@(compile-set-variable (caadr expr) env non-terminal-status)
		    ; on genere le code de la valeur de fin
		    ,@(compile-main (car (cddadr expr)) env non-terminal-status)
		    ; on place la valeur de fin sur la pile
		    (PUSH A0)
		    ; debut de la boucle
		    ,label-boucle
		    ; on place la valeur de la variable dans A0
		    ,@(compile-get-variable (caadr expr) env new-status)

		    ; on recupere la valeur de fin
		    (POP A1)
		    ; si la valeur de la variable est plus grande que la valeur de fin
		    (JG A0 A1 ,label-fin)
		    ; on replace la valeur de fin sur la pile
		    (PUSH A1)
		    ; on genere le code du 'for'
		    ,@(multi-compile (cddr expr) env new-status)

		    ; on place la valeur de la variable dans A0
		    ,@(compile-get-variable (caadr expr) env new-status)
		    
		    ; on incremente A0
		    (ADD A0 1)
		    ; on affecte A0 a la variable; on a incremente la variable d'une unite
		    ,@(compile-set-variable (caadr expr) env new-status)

		    ; on saute au debut de la boucle
		    (JMP ,label-boucle)
		    ; fin de la boucle
		    ,label-fin))))
  )


; expr : une liste dont le premier element est 'cons'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-cons(expr env status)
  ; deux arguments donnes a 'cons'
  (if (= (length expr) 3)
      (let (; statut interdisant la recursivite terminale
	    (non-terminal-status (copy-status status :change t :terminal-recursion nil))
	    ; statut prenant en compte l'empilement d'un element sur la pile
	    (new-status (make-status :base (+ (get-base status) 1)
				     :current-env (get-current-env status)
				     :in-label (is-in-label status)
				     :terminal-recursion nil)))

	`(; on genere le code du futur 'cdr'
	  ,@(compile-main (caddr expr) env non-terminal-status)
	  (PUSH A0)
	  ; on genere le code du futur 'car'
	  ,@(compile-main (cadr expr) env new-status)
	  ; on place le 'cdr' dans A1
	  (POP A1)
	  ; construit une liste de 'car' A0 et de 'cdr' A1, et place
	  ; la liste dans A0
	  (CONS A0 A1)))

    ; sinon
    (error "compile-cons: pas assez d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premier element est 'car'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-car(expr env status)
  ; un argument donne a 'car'
  (if (= (length expr) 2)
      (let (; statut interdisant la recursivite terminale
	    (non-terminal-status (copy-status status :change t :terminal-recursion nil)))

	`(; on genere le code de l'argument
	  ,@(compile-main (cadr expr) env non-terminal-status)
	    ; place le 'car' de la liste dans A0, dans A0
	    (CAR A0)))

    ; sinon
    (error "compile-car: mauvais nombre d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premierr element est 'cdr'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-cdr(expr env status)
  ; un argument donne a 'cdr'
  (if (= (length expr) 2)
       (let (; statut interdisant la recursivite terminale
	     (non-terminal-status (copy-status status :change t :terminal-recursion nil)))

	 `(; on genere le code de l'argument
	   ,@(compile-main (cadr expr) env non-terminal-status)
	     ; place le 'cdr' de la liste dans A0, dans A0
	     (CDR A0)))
    
    ; sinon
    (error "compile-cdr: mauvais nombre d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premier element est 'list'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-list(expr env status)
  (labels
   ; expr : une expression de la forme '(list expr1..exprn)'

   ; resultat : renvoie l'expression equivalente sous la forme '(cons expr1 (cons expr2..(cons exprn nil)..))
   ((convert-list-to-cons(expr)
			 (if expr
			     `(cons ,(car expr)
				    ,(convert-list-to-cons (cdr expr)))
			   nil)))

   ; au moins un argument donne a 'list'
    (if (cdr expr)
	(let (; statut interdisant la recursivite terminale
	      (non-terminal-status (copy-status status :change t :terminal-recursion nil)))
	  ; compile l'expression transformee
	  (compile-main (convert-list-to-cons (cdr expr)) env non-terminal-status))

      ; sinon, pas d'argument
      `(MOVEI A0 nil)))
  )


; expr : une liste dont le premierr element est 'list*'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-list*(expr env status)
  (case (length expr)
	; pas d'argument donne a 'list*'
	(1    	(error "compile-list*: trop peu d'arguments (recu : ~W)" expr))

	; un argument donne a 'list'*'
	(2     	(compile-main (cadr expr) env status))

	; au moins deux arguments donnes a 'list*'
	(t     	(let (; statut interdisant la recursivite terminale
		      (non-terminal-status (copy-status status :change t :terminal-recursion nil)))
		  (compile-main (list*-to-cons expr) env non-terminal-status))))
  )


; expr : une liste dont le premier element est '+'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-add(expr env status)
  (labels
   ; lexpr : une liste d'expressions
   ; status : un statut de compilation

   ; resultat : renvoie le code qui effectue l'addition des expressions de la liste, plus A0
   ((aux(lexpr status)
	; plus d'expression
	(if (null lexpr)
	    nil

	  `(; on sauve A0
	    (PUSH A0)
	    ; on compile la premiere expression
	    ,@(compile-main (car lexpr) env status)
	    ; on place le resultat dans A1
	    (MOVE A1 A0)
	    ; on recupere A0
	    (POP A0)
	    ; on ajoute A0 et A1 avec le resultat dans A0
	    (ADD A0 A1)
	    ; on fait de meme pour les expressions restantes
	    ,@(aux (cdr lexpr) status)))))

   (let (; nombre d'arguments donnes a '+'
	 (count (length (cdr expr))))
     (case count
	   ; pas d'argument donne
	   (0 `((MOVEI A0 0)))

	   ; un argument donne a '+'
	   (1 (compile-main (cadr expr) env status))

	   ; au moins deux arguments
	   (t (let (; statut interdisant la recursivite terminale
		    (non-terminal-status (copy-status status :change t :terminal-recursion nil))
		    ; statut prenant en compte l'empilement d'un element et interdisant
		    ; la recursivite terminale
		    (new-status (make-status :base (+ (get-base status) 1)
					     :current-env (get-current-env status)
					     :in-label (is-in-label status)
					     :terminal-recursion nil)))
		`(; on compile le premier argument avec le resultat dans A0
		  ,@(compile-main (cadr expr) env non-terminal-status)
		    ,@(aux (cddr expr) new-status)))))))
  )


; expr : une liste dont le premier element est '1+'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-increment(expr env status)
  ; un argument donne a '1+'
  (if (= (length expr) 2)
      (compile-add `(+ 1 ,(cadr expr)) env status)

    ; sinon
    (error "compile-increment: mauvais nombre d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premier element est '-'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-sub(expr env status)
  (labels
   ; lexpr : une liste d'expressions
   ; status : un statut de compilation

   ; resultat : renvoie le code qui effectue les soustractions successives des expressions de la liste,
   ; soustraite a A0
   ((aux(lexpr status)
	(if (null lexpr)
	    nil

	  `(; on sauve A0
	    (PUSH A0)
	    ; on compile le premier argument
	    ,@(compile-main (car lexpr) env status)
	    ; on place le resultat dans A1
	    (MOVE A1 A0)
	    ; on recupere A0
	    (POP A0)
	    ; on soustrait A1 de A0 avec le resultat dans A0
	    (SUB A0 A1)
	    ; on fait de meme pour les expressions restantes
	    ,@(aux (cdr lexpr) status)))))
   
   (let ((count (length (cdr expr))))
     (case count
	   ; pas d'argument donne a '-'
	   (0 (error "compile-sub: trop peu d'arguments (recu : ~W)" expr))

	   ; un argument donne a '-'
	   (1 (compile-sub `(- 0 ,(cadr expr)) env status))

	   ; au moins deux arguments donnes a '-'
	   (t (let (; statut interdisant la recursivite terminale
		    (non-terminal-status (copy-status status :change t :terminal-recursion nil))
		    ; statut prenant en compte l'empilement d'un element et interdisant
		    ; la recursivite terminale
		    (new-status (make-status :base (+ (get-base status) 1)
					     :current-env (get-current-env status)
					     :in-label (is-in-label status)
					     :terminal-recursion nil)))
		`(; on compile le premier argument avec le resultat dans A0
		  ,@(compile-main (cadr expr) env non-terminal-status)
		    ,@(aux (cddr expr) new-status)))))))
  )


; expr : une liste dont le premier element est '1-'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-decrement(expr env status)
  ; un argument donne a '1-'
  (if (= (length expr) 2)
      (compile-sub `(- ,(cadr expr) 1) env status)
    
    ; sinon
    (error "compile-decrement: mauvais nombre d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premier element est '*'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'


(mdefun compile-mul(expr env status)
  (labels
   ; lexpr : une liste d'expressions
   ; status : un statut de compilation

   ; resultat : renvoie le code qui effectue les multiplications successives des expressions de la liste,
   ; multiplie par A0
   ((aux(lexpr status)
	(if (null lexpr)
	    nil

	  `(; on sauve A0
	    (PUSH A0)
	    ; on compile le premier argument
	    ,@(compile-main (car lexpr) env status)
	    ; on place le resultat dans A1
	    (MOVE A1 A0)
	    ; on recupere A0
	    (POP A0)
	    ; on multiplie A0 et A1, avec le resultat dans A0
	    (MUL A0 A1)
	    ; on fait de meme pour les expressions restantes
	    ,@(aux (cdr lexpr) status)))))

   (let ((count (length (cdr expr))))
     (case count
	   ; pas d'argument donne a '*'
	   (0 (compile-main 1 env status))

	   ; un argument donne a '*'
	   (1 (compile-main (cadr expr) env status))

	   ; au moins deux arguments donnes a '*'
	   (t (let (; statut interdisant la recursivite terminale
		    (non-terminal-status (copy-status status :change t :terminal-recursion nil))
		    ; statut prenant en compte l'empilement d'un element et interdisant
		    ; la recursivite terminale
		    (new-status (make-status :base (+ (get-base status) 1)
					     :current-env (get-current-env status)
					     :in-label (is-in-label status)
					     :terminal-recursion nil)))
		`(; on compile le premier argument avec le resultat dans A0
		  ,@(compile-main (cadr expr) env non-terminal-status)
		  ,@(aux (cddr expr) new-status)))))))
   )


; expr : une liste dont le premier element est '*'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-div(expr env status)
  (labels
   ; lexpr : une liste d'expressions
   ; status : un statut de compilation

   ; resultat : renvoie le code qui effectue les divisions successives des expressions de la liste,
   ; en partanr de A0
   ((aux(lexpr status)
	(if (null lexpr)
	    nil

	  `(; on sauve A0
	    (PUSH A0)
	    ; on compile le premier argument
	    ,@(compile-main (car lexpr) env status)
	    ; on place le resultat dans A1
	    (MOVE A1 A0)
	    ; on recupere A0
	    (POP A0)
	    ; on divise A0 par A1, avec le resultat dans A0
	    (DIV A0 A1)
	     ; on fait de meme pour les expressions restantes
	    ,@(aux (cdr lexpr) status)))))
   
   (let ((count (length (cdr expr))))
     (case count
	   ; pas d'argument donne a '/'
	   (0 (error "compile-div: trop peu d'arguments (recu : ~W)" expr))

	   ; un argument donne a '/'
	   (1 (compile-div `(/ 1 ,(cadr expr)) env status))

	   ; au moins deux arguments donnes a '/'
	   (t (let (; statut interdisant la recursivite terminale
		    (non-terminal-status (copy-status status :change t :terminal-recursion nil))
		    ; statut prenant en compte l'empilement d'un element et interdisant
		    ; la recursivite terminale
		    (new-status (make-status :base (+ (get-base status) 1)
					     :current-env (get-current-env status)
					     :in-label (is-in-label status)
					     :terminal-recursion nil)))
		`(; on compile le premier argument avec le resultat dans A0
		  ,@(compile-main (cadr expr) env non-terminal-status)
		    ,@(aux (cddr expr) new-status)))))))
  )


; expr : une liste dont le premier element est '<', '<=', '>', '>=' ou '='
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-number-comparison(expr env status)
  (labels (; name : nom d'une instruction de comparaison entre entiers LISP

	   ; resultat : renvoie le nom de l'instruction de la MACHINE VIRTUELLE correspondante
	   (map-to-instruction (name)
			       (let* ((mapping-table '((<	. LT)
						      (<=	. LE)
						      (=	. EQ)
						      (>	. GT)
						      (>=	. GE)))
				      (r (assoc name mapping-table)))
				 (if r
				     (cdr r)
				   nil))))
				 
	  (case (length (cdr expr))
		; pas d'argument ou un argument donne a l'operateur
		((0 1)	(error "compile-number-comparison: trop peu d'arguments (recu : ~W)" expr))

		; deux arguments donnes a l'operateur
		(2	(let (; statut interdisant la recursivite terminale
			      (non-terminal-status (copy-status status :change t :terminal-recursion nil))
			      ; statut prenant en compte l'empilement d'un element sur la pile
			      (new-status (make-status :base (+ (get-base status) 1)
						       :current-env (get-current-env status)
						       :in-label (is-in-label status)
						       :terminal-recursion nil)))
							
		     
			  `(; on compile le premier argument avec le resultat dans A0
			    ,@(compile-main (cadr expr) env non-terminal-status)
			    (PUSH A0)
			    ; on compile le deuxieme argument
			    ,@(compile-main (caddr expr) env new-status)
			    (POP A1)
			    ; on recupere la valeur du premier argument
			    ; on compare les deux arguments
			    (,(map-to-instruction (car expr)) A1 A0))))
	 
		; au moins deux arguments
		(t (error "compile-number-comparison: operateur ~W non binaire non gere (recu : ~W)" (car expr) expr))))
  )


; expr : une liste dont le premier element est 'not'
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-not(expr env status)
  ; un argument donne a 'not'
  (if (= (length expr) 2)
      (let (; statut interdisant la recursivite terminale
	    (non-terminal-status (copy-status status :change t :terminal-recursion nil)))

	`(; on compile l'argument avec le resultat dans A0
	  ,@(compile-main (cadr expr) env non-terminal-status)
	  ; on inverse la valeur de verite dans A0
	  (NOT A0)))

    (error "compile-not: mauvais nombre d'arguments (recu : ~W)" expr))
  )


; expr : une liste dont le premier element est un nom de fonction ou de macro
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

; rem : la fonction appelee doit etre locale, ou l'appel doit recursif direct, ou la fonction appelee
; doit etre mdefinie et etre compilee puis chargee dans la MACHINE VIRTUELLE lors de l'execution de l'appel.
; En fait, si la fonction mdefinie ne comporte pas de mots-cles, il n'est pas obligatoire qui celle-ci
; soit compilee et chargee lors de l'execution de l'appel

(mdefun compile-call(expr env status)
  (labels
   ; lval : une liste d'expressions (initialement, les arguments de l'appel)
   ; status : un statut de compilation
   ; is-macro : indique si on compile un appel a une fonction ou a une macro

   ; resultat : renvoie le code compile qui empile les valeurs des expressions
   ; dans l'ordre de 'lval'
   ((compile-arguments(lval status is-macro)
		      (if lval
			  (let (; on cree un statut de compilation qui interdit la recursivite
				; terminale et qui tient compte de l'empilement d'un element
				(new-status (make-status :base (+ (get-base status) 1)
							 :current-env (get-current-env status)
							 :in-label (is-in-label status)
							 :terminal-recursion nil)))
			    (cond
			     ; appel a une macro
			     (is-macro	`(; on empile l'argument sans le compiler
					  (MOVEI A0 ,(car lval))
					  (PUSH A0)
					  ,@(compile-arguments (cdr lval) new-status is-macro)))

			     ; appel a une fonction
			     (t		`(; on compile l'evaluation de l'argument
					  ,@(compile-main (car lval) env status)
					  (PUSH A0)
					  ,@(compile-arguments (cdr lval) new-status is-macro)))))
			
			nil))

    ; level : un entier qui represente le nombre de niveaux qui separent niveau courant dans l'environnement
    ; des fonctions du niveau de la fonction qu'on appelle; lorsqu'on appelle une fonction englobante,
    ; 'level' est negatif; lorsqu'on appelle une fonction situee au meme niveau, 'level' est nul; lorsqu'on
    ; appelle une fonction de niveau inferieur, ce qui est le cas lorsqu'on saute de corps d'un 'labels'
    ; vers une des fonctions du 'labels', 'level' est positif et ne peut valoir qu'un

    ; resultat : renvoie le code compile qui fourni l'adresse du bloc de pile englobant au
    ; sommet de la pile, necessaire pour construire le bloc d'appel
    (get-previous-env(level)
		     (cond
		      ; on saute vers une fonction situee au meme niveau
		      ; dans l'environnement des fonctions
		      ((= level 0)	`(; on recupere l'adresse du bloc de pile englobant
					  ; le bloc de pile courant
					  (LOCAL A0 1)
					  (PUSH A0)))

		      ; on saute de corps d'un 'labels' vers une de ses fonctions
		      ((> level 0)	`(; on recupere l'adresse du bloc de pile courant
					  ; qui sera le bloc de pile englobant du bloc d'appel genere
					  (PUSH FP)))

		      ; on saute vers une fonction englobante
		      (t		(let (; etiquette indiquant le debut de la boucle
					      (boucle (gentemp))
					      ; etiquette indiquant la fin de la boucle
					      (fin (gentemp)))
					  
					  `(; on sauve FP
					    (MOVE A1 FP)
					    ; nombre de niveaux a remonter
					    (MOVEI A0 ,level)
					    ; debut de la boucle
					    ,boucle
					    ; si on est dans le bon bloc de pile, on sort de la boucle
					    (JEQ A0 0 ,fin)
					    ; on recupere l'adresse du bloc de pile englobant
					    (LOCAL FP 1)
					    ; on est remonte d'un niveau
					    (ADD A0 1)
					    ; on continue
					    (JMP ,boucle)
					    ; fin de la boucle
					    ,fin
					    ; on recupere l'adresse de l'environnement englobant
					    (LOCAL A0 1)
					    ; on restaure FP
					    (MOVE FP A1)
					    ; on empile l'adresse du bloc de pile cherche
					    (PUSH A0))))))

    ; env-offset : nil ou un entier

    ; resultat : si 'env-offset' est nil, on fait un appel en dehors de toute fonction, et on peut
    ; etre dans un bloc de pile virtuel si des variables locales on deja ete empilees (voir 'let');
    ; sinon on fait un appel a partir d'une fonction a une fonction qui est dans la portee syntaxique
    ; de l'appel, et 'env-offset' represente alors le nombre de niveaux qui separent la fonction dans
    ; laquelle se trouve le code d'appel et la fonction vers laquelle on saute
    (push-previous-env(env-offset)
		      ; saut entre deux fonctions dont l'une est dans la portee syntaxique de l'autre
		      (cond
		       (env-offset
			 	(get-previous-env env-offset))

		       ; saut en dehors de tout fonction, a partir d'un bloc virtuel (voir 'let')
		       ((> (get-base status) 0)
				`((PUSH FP)))

		       ; sinon, saut en dehors de toute fonction et en dehors d'un bloc virtuel
		       (t	`((MOVEI A0 nil)
				  (PUSH A0))))))


   (let* (; nom de la fonction appelee
	  (name (car expr))
	  ; on recupere le prototype de la fonction appelee; si 'prototype' vaut nil
	  ; alors on appelle une fonction qui n'est pas dans la portee syntaxique
	  ; du code d'appel, sinon on appelle une fonction qui est dans la portee
	  ; syntaxique i.e. on fait un appel recursif ou un appel a une fonction locale7
	  (prototype (copy-tree (get-function-prototype env name)))
	  ; on recupere la liste des arguments
	  (lval (cdr expr))
	  ; etiquette indiquant le debut de la fonction; par defaut, on prend
	  ; le nom de la fonction
	  (jump-label (car expr))
	  ; on recupere le nombre de niveaux qui separent le bloc courant
	  ; du niveau qui contient la fonction appelee
	  (env-offset (get-env-offset env status name))
	  ; on cherche si on est dans le corps d'une fonction
	  (in-function (and (get-current-env status)
			    (car (get-current-env status))))
	  ; on cree un statut interdisant la recursivite terminale
	  (non-terminal-status (make-status :base (get-base status)
					    :current-env (get-current-env status)
					    :in-label (is-in-label status)
					    :terminal-recursion nil))
	  ; indique si on appelle une macro ou une fonction; par defaut, on considere
	  ; qu'on appelle une fonction
	  (is-macro nil)
	  ; indique si on fait un appel recursif ou un appel a une fonction locale,
	  ; ou non; par defaut, on considere qu'on fait un appel recursif ou un appel
	  ; a une fonction locale; utile pour le traitement du bloc de pile englobant
	  (is-local t))

     ; on fait un appel recursif ou un appel a une fonction locale
     (if prototype
	 ; on recupere l'etiquette indiquant le debut de la fonction
	 (setf jump-label (get-function-symbol env name))

       ; sinon
       (let (; on cherche la definition d'une fonction ou macro compilee
	     ; 'name' dans l'environnement du META-EVALUATEUR
	     (func (get-compiled-function name)))
	 ; on indique que la fonction n'est pas dans la portee syntaxique
	 ; du code d'appel
	 (setf is-local nil)

	 (cond
	  ; il existe une fonction compilee ou macro 'name' definie dans l'environnement du META-EVALUATEUR
	  (func ; on recupere le prototype de la fonction appelee
	   	(setf prototype (copy-tree (extract-prototype func)))
		; on note si c'est une fonction ou une macro
		(setf is-macro (macrop func)))

	  ; sinon, s'il existe une fonction ou macro mdefinie 'name'
	  ((setf func (get-mdefined-function name))	     
		 ; on recupere sa liste de parametres et on essaie de construire
		 ; un prototype a partir de cette liste de parametres
	   	(setf prototype (make-function-prototype (extract-prototype func)))
		; on note si c'est une fonction ou une macro
	       	(setf is-macro (macrop func))))))

     ; la fonction appelee est connue
     (if prototype
	 ; on essaie d'associer les arguments et le prototype
	 ; les arguments et le prototype ne correspondent pas
	 (if (null (assoc-prototype-arguments prototype lval))
	     (error "compile-call: les parametres ne correspondent pas au prototype de la fonction appelee (recu : ~W)" expr)
	   ; sinon, on recupere la liste des arguments adaptee au prototype
	   (setf lval (get-prototype-values prototype))))


     ; si la fonction appelee n'est pas connue, il s'agit alors d'un appel a une fonction
     ; LISP, et les valeurs de 'lval' et de 'is-local' sont adpatees a ce cas

     (cond
      ; on peut generer un appel terminal
      ((and ; 'status' indique que le resultat de la fonction appelee
	    ; n'est pas utilise par le code appelant
	    (is-terminal-recursion status)
	    ; on verifie qu'on est dans une fonction, et donc
	    ; qu'on va pouvoir ecraser son bloc d'appel par le bloc
	    ; d'appel qu'on va generer
	    in-function
	    ; on verifie qu'en ecrasant le bloc d'appel, on ne prive
	    ; pas le code appele de l'acces au parametres ou variables
	    ; locales de l'appelant
	    (or ; on appelle une fonction qui ne partage pas son environnement
	     	; avec la fonction appelante, donc pas de risque
	     	(null env-offset)
		; sinon, on fait appel a une fonction englobante, et donc qui ne
		; peut pas acceder aux parametres ou variables locales de la
		; fonction appelante
		(<= env-offset 0)))

       		(let (; etiquette indiquant le debut de la boucle
		      ; qui permet de deplacer le bloc d'appel
		      ; nouvellement cree a la place du bloc d'appel
		      ; de la fonction appelante
		      (boucle (gentemp))
		      ; etiquette indiquant la fin de cette meme boucle
		      (fin (gentemp)))

		  `(; on compile les arguments qui se retrouvent sur la pile
		    ,@(compile-arguments lval non-terminal-status is-macro)
		      ; on indique le nombre de parametres
		      (MOVEI A0 ,(length lval))
		      (PUSH A0)
		      ; on empile l'adresse du bloc d'appel de l'environnement englobant
		      ,@(push-previous-env env-offset)
		      ; on garde la meme adresse pour la base du bloc d'appel
		      (LOCAL A4 2)
		      (PUSH A4)
		      ; on garde la meme adresse pour le bloc d'appel precedent
		      (LOCAL A0 3)
		      (PUSH A0)
		      ; on garde la meme valeur de retour
		      (LOCAL A0 4)
		      (PUSH A0)
		 
		      ; A5 recoit l'adresse du bloc d'appel de l'appelant
		      (MOVE A5 A4)
		      ; nombre d'arguments empiles dans A3
		      (MOVEI A3 ,(length lval))
		      (SUB A5 A3)
		      ; A5 pointe maintenant sur la case qui contiendra le nombre d'arguments
		      ; du bloc d'appel de l'appele apres le deplacement de celui-ci
		      (SUB A5 1)
		     

		      ; A0 pointe sur la base du bloc d'appel de l'appelant
		      (MOVE A0 A4)
		      ; A0 pointe sur la premiere case du bloc d'appel
		      (SUB A0 1)

		      ; A1 pointe sur le sommet de la pile i.e. sur le dernier
		      ; element du bloc d'appel de l'appele qui est la valeur de retour
		      (MOVE A1 SP)
		      ; A1 pointe sur le nombre d'arguments du bloc d'appel de
		      ; l'appele
		      (ADD A1 4)
		      ; A1 pointe maintenant sur le premier element du bloc d'appel
		      ; de l'appele i.e. le premier argument
		      (ADD A1 A3)

		      ; A3 contient le nombre d'elements composant le bloc
		      ; d'appel de l'appele i.e. le nombre d'elements a deplacer
		      (ADD A3 5)
		      
		      ; debut du deplacement du bloc d'appel de l'appele
		      ,boucle
		      ; on a tout copie alors on saute apres la boucle
		      (JLE A3 0 ,fin)

		      ; on lit un element du bloc d'appel de l'appelant
		      (PEEK A2 A1)
		      ; on l'ecrit a la place du bloc d'appel de l'appelant
		      (POKE A0 A2)

		      ; on avance d'un element
		      (SUB A0 1)
		      (SUB A1 1)

		      ; un element de moins a deplacer
		      (SUB A3 1)
		      ; on continue
		      (JMP ,boucle)
		      ; fin du deplacement
		      ,fin

		      ; A0 pointe juste apres le dernier element du cadre de l'appele;
		      ; on le fait pointer sur le dernier element
		      (ADD A0 1)
		      ; on met a jour SP
		      (MOVE SP A0)
		      ; on met a jour FP
		      (MOVE FP A5)

		      ; on saute a la fonction appelante sans empiler la valeur de retour
		      ; puisqu'on la deja mise sur la pile
		      (JMP ,jump-label))))

      ; sinon, pas d'appel terminal possible
      (t	`(; on compile les arguments qui se retrouvent sur la pile
		  ,@(compile-arguments lval non-terminal-status is-macro)
		    ; on indique le nombre de parametres
		    (MOVEI A0 ,(length lval))
		    (PUSH A0)
		    ; on empile l'adresse du bloc d'appel de l'environnement englobant
		    ,@(push-previous-env env-offset)
		    ; on sauve FP
		    (MOVE A1 FP)
		    ; FP pointe sur l'adresse du bloc d'appel de l'environnement englobant
		    ; du nouveau bloc d'appel
		    (MOVE FP SP)
		    ; FP pointe sur le nombre d'arguments du nouveau bloc d'appel
		    (ADD FP 1)

		    (MOVE A2 SP)
		    ; A2 pointe sur la base du nouveau bloc d'appel
		    (ADD A2 ,(+ (length lval) 2))
		    ; on empile la valeur de SP avant le debut de l'empilement
		    (PUSH A2)
		    ; on empile l'adresse du bloc d'appel precedent
		    (PUSH A1)
		    ; on saute a la fonction appelee
		    (JSR ,jump-label))))))
  )


; expr : une lambda-expression
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-lambda(expr env status)
  ; prototype : un prototype

  ; resultat : une liste de couples '(variable valeur)' correspondant au prototype
  (labels ((make-env(prototype)
		    (let ((rest (get-rest prototype)))
		      (append (get-other prototype)
			      (get-optional prototype)
			      (get-key prototype)
			      (if rest
				  (list rest)
				nil)))))
	  
	  (let ((prototype (make-function-prototype (cadar expr))))

	    (cond
	     ; la liste des parametres est invalide
	     ((null prototype)
	      		(error "compile-lambda: ~W n'est pas une liste de parametres valide (recu : ~W)" (cadar expr) expr))

	     ; sinon
	     (t 	; on tente d'associer le prototype et les arguments
	      		(if (assoc-prototype-arguments prototype (cdr expr))
			    ; on transforme l'application de la lambda-expression en un 'let'
			    ; et on compile le resultat
			    (compile-main `(let ,(make-env prototype) ,(caddar expr)) env status)

			  ; si la liste des arguments et le prototype ne correpondant pas
			  (error "compile-lambda: la liste d'arguments ne correspond pas au prototype (recu : ~W)" expr))))))
  )


; expr : une variable
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-get-variable(expr env status)
  (labels (; level : un entier > 0 qui correspond au nombre de blocs d'appel
	   ; englobants qu'il faut 'remonter' pour atteindre la variable.
	   ; Cela se fait grace au pointeur sur l'environnement englobant du bloc d'appel

	   ; resultat : renvoie le code compile qui affecte l'adresse du 'level'-ieme bloc d'appel englobant
	   ; a FP et sauvegarde la valeur courante de FP dans A3
	   (jump-to-level(level)
			 (cond
			  ; on remonte d'un niveau
			  ((= level 1) `(; on sauve FP
					 (MOVE A3 FP)
					 ; on recupere l'adresse du bloc d'appel englobant
					 (LOCAL A0 1)
					 ; on fait pointer FP sur le bloc d'appel englobant
					 (MOVE FP A0)))
			  
			  (t (let (; etiquette indiquant le debut de la boucle
				   (boucle (gentemp))
				   ; etiquette indiquant la fin de la boucle
				   (fin (gentemp)))

			       `(; on sauve FP
				 (MOVE A3 FP)
				 ; on place le nombre de blocs d'appel a remonter dans A1
				 (MOVEI A1 ,level)
				 ; debut de la boucle
				 ,boucle
				 ; si on est dans le bon bloc d'appel, on saute apres la boucle
				 (JEQ A1 0 ,fin)
				 ; on recupere l'adresse du bloc d'appel englobant du bloc dans lequel
				 ; on se trouve
				 (LOCAL A0 1)
				 ; on fait pointer FP sur le bloc d'appel englobant
				 (MOVE FP A0)
				 ; on est monte d'un niveau
				 (SUB A1 1)
				 ; on recommence
				 (JMP ,boucle)
				 ; fin de la remontee
				 ,fin))))))

	  ; 'expr' est une variable, ce qui devrait toujours etre le cas
	  (if (is-variable expr)
	      (let (; on recupere le decalage associe a la variable
		    (offset (get-variable-offset env expr)))
		(cond
		 ; la variable est locale
		 (offset	(let (; on recupere le niveau de la variable dans l'environnement
				      ; des variables
				      (level (get-variable-level env expr)))
				  ; la variable est dans le bloc d'appel courant
				  (if (= level 0)
				    `((LOCAL A0 ,offset))
			   
				    `(; sinon, la variable est dans un bloc d'appel d'une fonction
				      ; englobante; on doit donc remonter de 'level' niveaux pour acceder
				      ; au bloc d'appel qui contient la variable
				      ; on fait pointer FP sur le 'bon' bloc d'appel
				      ,@(jump-to-level level)
				      ; on recupere la valeur de la variable
				      (LOCAL A0 ,offset)
				      ; on restaure FP
				      (MOVE FP A3)))))
			 
		 ; la variable est globale
		 (t		`((GLOBAL A0 ,expr)))))

	    (t (error "compile-get-variable: ~W n'est pas une variable" expr))))
  )


; expr : une variable
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-set-variable(expr env status)
  (labels (; level : un entier > 0 qui correspond au nombre de blocs d'appel
	   ; englobants qu'il faut 'remonter' pour atteindre la variable.
	   ; Cela se fait grace au pointeur sur l'environnement englobant du bloc d'appel

	   ; resultat : renvoie le code compile qui affecte l'adresse du 'level'-ieme bloc d'appel englobant
	   ; a FP et sauvegarde la valeur courante de FP dans A3
	   (jump-to-level(level)
			(cond
			  ; on remonte d'un niveau
			  ((= level 1) `(; on sauve FP
					 (MOVE A3 FP)
					 ; on recupere l'adresse du bloc d'appel englobant
					 (LOCAL A0 1)
					 ; on fait pointer FP sur le bloc d'appel englobant
					 (MOVE FP A0)))
			  
			  (t (let (; etiquette indiquant le debut de la boucle
				   (boucle (gentemp))
				   ; etiquette indiquant la fin de la boucle
				   (fin (gentemp)))

			       `(; on sauve FP
				 (MOVE A3 FP)
				 ; on place le nombre de blocs d'appel a remonter dans A1
				 (MOVEI A1 ,level)
				 ; debut de la boucle
				 ,boucle
				 ; si on est dans le bon bloc d'appel, on saute apres la boucle
				 (JEQ A1 0 ,fin)
				 ; on recupere l'adresse du bloc d'appel englobant du bloc dans lequel
				 ; on se trouve
				 (LOCAL A0 1)
				 ; on fait pointer FP sur le bloc d'appel englobant
				 (MOVE FP A0)
				 ; on est monte d'un niveau
				 (SUB A1 1)
				 ; on recommence
				 (JMP ,boucle)
				 ; fin de la remontee
				 ,fin))))))

	  ; 'expr' est une variable, ce qui devrait toujours etre le cas	  
	  (if (is-variable expr)
	      (let (; on recupere le decalage associe a la variable
		    (offset (get-variable-offset env expr)))

		(cond
		 (offset (let (; on recupere le niveau de la variable dans l'environnement
			       ; des variables
			       (level (get-variable-level env expr)))

			   (if (= level 0)
			       ; la variable est dans le bloc d'appel courant
			       `((SETLOCAL ,offset A0))

			   `(; sinon, la variable est dans un bloc d'appel d'une fonction
			     ; englobante; on doit donc remonter de 'level' niveaux pour acceder
			     ; au bloc d'appel qui contient la variable
			     ; on fait pointer FP sur le 'bon' bloc d'appel
			     ,@(jump-to-level level)
			     ; on recupere la valeur de la variable  
			     (LOCAL A0 ,offset)
			     ; on restaure FP
			     (MOVE FP A3)))))

		 ; la variable est globale		 
		 (t `((SETGLOBAL ,expr A0)))))

	    (error "compile-set-variable: ~W n'est pas une variable" expr)))
  )


; expr : une constante
; env : un environnement de compilation
; status : un statut de compilation

; resultat : renvoie le code compile de 'expr' selon 'env' et 'status'

(mdefun compile-constant(expr env status)
  ; place la constante dans A0
  `((MOVEI A0 ,expr))
  )
