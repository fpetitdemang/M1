;Compile un fichier : ouvre un fichier en lecture et via la fontion compiler, compile le code
;Prend en paramettre un nom de fichier
(defun compilerFichier (fich)
	(let 
		(
			(strim (open fich :direction :input)) 
	 		(liste ()) 
	 		(code ())
		)
		(loop while (not (eql (setf liste (read strim nil :EOF)) ':EOF))
			do	(
					setf code (append code (compiler liste))
				)
		)
		(close strim)
		code
	)
)

;Prermet de "compiler" le code lisp en ASM et plus exactement de faire le lien avec les
;fonctions de compilation.
;Nous utilisons un environement (env) un environement fonctionnnel (fenv)
;et le com de la fonction (nomf)
(defun compiler (exp &optional (env ()) ( fenv ()) (nomf ()) )
	(cond	
		((atom exp)(compilLitteral exp env fenv nomf))
		((consp (car exp)) (if (eql (caar exp) 'lambda ) (compilLambda exp env fenv nomf)))
		((member (car exp) '(+ - * /)) (compilOp exp env fenv nomf))
		((member (car exp) '(< > = <= >= )) (compilTest exp env fenv nomf))
		((eql (car exp) 'QUOTE) (compilQuote (cdr exp) env fenv nomf))
		((eql (car exp) 'IF ) (compilIf (cdr exp)env fenv nomf))
		((eql (car exp) 'COND ) (compilCond (cdr exp) (gensym "FINCOND")env fenv nomf ))
		((eql (car exp) 'PROGN ) (compilProgn (cdr exp) env fenv nomf ))
		((eql (car exp) 'LOOP ) (compilLoop (cdr exp) env fenv nomf ))
		((eql (car exp) 'AND ) (compilAnd (cdr exp) (gensym "FINAND") env fenv nomf))
		((eql (car exp) 'OR ) (compilOr (cdr exp) (gensym "FINOR") env fenv nomf))
		((eql (car exp) 'SETF ) (compilSetf (cdr exp) env fenv nomf))
		((eql (car exp) 'LET ) (compilLet (cdr exp) env fenv nomf))
		((eql (car exp) 'DEFUN ) (compilDefun (cdr exp) env fenv nomf))
		((eql (car exp) 'LABELS ) (compilLabels (cdr exp) env fenv nomf))
		((macro-function (car  exp)) (compiler (macroexpand exp) env fenv nomf)) ;si c'est une macro, on l'expand et on la compile
		(`(function ,(car exp)) (compilAppel exp env fenv nomf))
	)
)

;charge un litteral dans R0
;si le litteral estune variable (dans env) on charge sa valeur, 
;du moins un code permettant a la VM de charger la valeur => MOVE (LOC deplacement NiveauEmpilement) R0
;sinon, c'est une constante => ex : MOVE #5 R0 
(defun compilLitteral(exp env fenv nomf )
	(let ((var (assoc exp env)) )
		(if var `((MOVE ,(cdr var) (:Rx :R0))) `((MOVE (:DIESE ,exp) (:Rx :R0)))
		)
	)
)

;On compile les operateurs + - * / et de plus on concataine les operateur
(defun compilOp (exp env fenv nomf)
	(let	(
			(op (car exp))
		)
		(cond
			((null (cdddr exp)) 
				(append (compiler (cadr exp) env fenv nomf)
					'((PUSH (:Rx :R0)))
					(compiler (caddr exp)env fenv nomf)
					'((PUSH (:Rx :R0)))
					'((POP (:Rx :R1)))
					'((POP (:Rx :R0)))
					(cond
						((eql op '+ ) '((ADD (:Rx :R1) (:Rx :R0)))) 
							((eql op '- ) '((SUB (:Rx :R1) (:Rx :R0))))
							((eql op '* ) '((MULT (:Rx :R1) (:Rx :R0))))
							((eql op '/ ) '((DIV (:Rx :R1) (:Rx :R0))))
					)
				)
			)
			(t (append 
				(compiler  `(,op ,(list op (cadr exp) (caddr exp)) ,@(cdddr exp)) env fenv nomf ))
			)
		)
	)
)

;on compile les instruction de test (= < > <= >=)
(defun compilTest ( exp env fenv nomf)
	(let	(
			(op (car exp))
			(fin (gensym "finTest"))
		)
		(append 
			(compiler (cadr exp)env fenv nomf)
			'((PUSH (:Rx :R0)))
			(compiler (caddr exp)env fenv nomf)
			'((PUSH (:Rx :R0)))
			'((POP (:Rx :R0)))
			'((POP (:Rx :R1)))
			'((CMP (:Rx :R1) (:Rx :R0)))
			'((MOVE (:DIESE T) (:Rx :R0)))
			(cond
				((eql op '= )  `((JEQ (@ ,fin))))
				((eql op '< )  `((JL (@ ,fin))))
				((eql op '> )  `((JG (@ ,fin))))
				((eql op '<= ) `((JLE (@ ,fin))))
				((eql op '>= ) `((JGE (@ ,fin))))
			)
			'((MOVE (:DIESE NIL) (:Rx :R0)))
			`((@ ,fin))
		)
	)
)

(defun compilQuote (exp env fenv nomf)
	`((MOVE ,exp (:Rx :R0)))
)

;on compile le if
(defun compilIf ( exp  env fenv nomf)
	(let 
		((sinon (gensym "SINON")) 
		(finSi (gensym "FINSI")))
		(append (compiler (car exp) env fenv nomf)
			'((CMP (:Rx :R0) (:DIESE NIL)))
			`((JEQ (@ ,sinon)))
			(compiler (cadr exp)  env fenv nomf)
			`((JMP (@ ,finSi)))
			`((@ ,sinon))
			(compiler (caddr exp) env fenv nomf)
			`((@ ,finSi))
		)
	)
)

;on compile le cond
(defun compilCond ( exp fin env fenv nomf)
	(if (null exp) 
		(append '((MOVE (:DIESE NIL) (:Rx :R0))) `((@ , fin)))
		(let	(
				(condn (gensym "CONDN"))
			)
			(append (compiler (caar exp) env fenv nomf) 
				'((CMP (:Rx :R0) (:DIESE NIL)))
				`((JEQ (@ ,condn)))
				(compiler (cadar exp) env fenv nomf) 
				`((JMP (@ ,fin)))
				`((@ ,condn))
				(compilCond (cdr exp) fin env fenv nomf)
			)
		)
	)
)


;on compile le progn
(defun compilProgn (exp env fenv nomf)
	(if (null exp) ()
		(append (compiler (car exp) env fenv nomf)
			(compilProgn (cdr exp) env fenv nomf)
		)
	)
)

;on compile les boucles LOOP while et until
(defun compilLoop ( exp env fenv nomf)      
	(cond
		((eql (car exp) 'WHILE) (compilWhile (cdr exp) env fenv nomf))
		((eql (car exp) 'UNTIL) (compilUntil (cdr exp) env fenv nomf))
	)
)

;on compile le while
(defun compilWhile (exp env fenv nomf) 
	(let	(
			(fin (gensym "FINWHILE"))
			(boucle (gensym "WHILE"))
		)
		(append
			`((@ ,boucle))
			(compiler (car exp) env fenv nomf)
			'((CMP (:Rx :R0) (:DIESE NIL)))
			`((JEQ (@ ,fin)))
			(compiler (caddr exp) env fenv nomf)
			`((JMP (@ ,boucle)))
			`((@ ,fin))
		)
	)
)


;on compile le until
(defun compilUntil (exp env fenv nomf)  
	(let	(
			(fin (gensym "FINUNTIL"))
			(boucle (gensym "UNTIL"))
		)
		(append
			`((@ ,boucle))
			(compiler (car exp) env fenv nomf)
			'((CMP (:Rx :R0) (:DIESE T)))
			`((JEQ (@ ,fin)))
			(compiler (caddr exp) env fenv nomf)
			`((JMP (@ ,boucle)))
			`((@ ,fin))
		)
	)
)

;on compile le ET binaire
;si une condition du ET est fausse, alors tout le ET est faux
(defun compilAnd (exp fin env fenv nomf)
	(if (null exp) 
		(append '((MOVE (:DIESE T) (:Rx :R0))) `((@ ,fin)))
		(append 
				(compiler (car exp) env fenv nomf)
				'((CMP (:Rx :R0) (:DIESE T)))
				`((JNE (@ ,fin)))
				(compilAnd (cdr exp) fin env fenv nomf)			
		)
	)
)

;on compile le OU binaire
;si une condition du OU est vraie alors tout le OU est vraie
(defun compilOr (exp fin env fenv nomf)
	(if (null exp) 
		(append '((MOVE (:DIESE NIL) (:Rx :R0))) `((@ ,fin)))
		(append
				(compiler (car exp) env fenv nomf)
				'((CMP (:Rx :R0) (:DIESE T)))
				`((JEQ (@ ,fin)))
				(compilOR (cdr exp) fin env fenv nomf)
		)
	)
)


;on compile le setf
(defun compilSetf (exp env fenv nomf)
	(if (null exp) ()
		(append
			(compiler (cadr exp) env fenv nomf)
			`((MOVE (:Rx :R0)  ,(cadar (compiler (car exp) env fenv nomf) ) ) )
			(compilSetf (cddr exp ) env fenv nomf)
		)
	)
)


;on compile le let
(defun compilLet (exp env fenv nomf)
	(let (  (nivEmp (assoc nomf fenv)) )
		(append 
			(compilLoc (car exp) env fenv nomf)
			(compiler (cadr exp) (localEnv (car exp) env 1 (cadr nivEmp)) fenv nomf)
			(depilLoc (car exp) env fenv nomf)
		)
	)
)

;on compile les variables locales (fonction utilisee par let) 
(defun compilLoc (exp env fenv nomf)
	(if (null exp) ()
		(append (compiler (cadar exp) env fenv nomf)	;on compile la valeur (dans RO)
			'((PUSH (:Rx :R0)))			;on l'empile
			(compilLoc (cdr exp) env fenv nomf )	;on compile la suivante
		)
	)
)

;on genere le code qui depile les variables locales (fonction utilisee par let)
(defun depilLoc (exp env fenv nomf)
	(if (null exp) ()
		(append 
			'((POP (:Rx :R1)))			;on depile la variable
			(depilLoc (cdr exp) env fenv nomf )	;on passe a la suivante
		)
	)
)

;on cree un environnement augmente des variables locales du let
(defun localEnv (exp env dep nivEmp)  
	(if (atom exp) env
		(localEnv (cdr exp) (cons (cons (caar exp)`(LOC ,dep ,nivEmp)) env) (+ 1 dep) nivEmp) 
	)
)

;on compile les lambda expressions
(defun compilLambda (exp env fenv nomf)
	(let	(
			(lambdexp (gensym "lambda"))
			(n (length (cdr exp)))
			(nivEmp (assoc nomf fenv))
		)
		(append
			(compilParam (cdr exp)  env fenv nomf)
			`((PUSH (:DIESE ,n)))
			`((MOVE (:Rx :FP) (:Rx :R1)))
			`((MOVE (:Rx :SP) (:Rx :FP)))
			`((MOVE (:Rx :SP) (:Rx :R2)))
			`((SUB  (:DIESE ,n) (:Rx :R2)))	
			`((SUB  (:DIESE 1) (:Rx :R2)))
			`((PUSH (:Rx :R2)))
			`((PUSH (:Rx :R1)))
			(if nivEmp  `((PUSH (:DIESE ,(+ 1 (cadr nivEmp)))))  `((PUSH (:DIESE ,0))))
			`((PUSH (:DIESE 0)))
			(compiler (caddar exp)
				(paramEnv (cadar exp) env 1 (if nivEmp   (+ 1 (cadr nivEmp)) 0)) 
				(fctEnv  (list (cons lambdexp (cdar exp))) fenv (if nivEmp (+ 1 (cadr nivEmp)) 0))
			 	lambdexp
			)
			`((MOVE ( 1 (:Rx :FP))(:Rx :SP)))
			`((MOVE ( 2 (:Rx :FP))(:Rx :FP)))
		)
	)
)

;on compile les definitions de fonctions
(defun compilDefun (exp env fenv nomf)
	(let	(
			(nivEmp (assoc nomf fenv))
		)
		(append
			'((FENTRY))
			`((@ ,(car exp)))
			(compilProgn (cddr exp)
			(paramEnv (cadr exp) env 1 (if nivEmp   (+ 1 (cadr nivEmp)) 0)) 
			(fctEnv (list exp ) fenv (if nivEmp (+ 1 (cadr nivEmp)) 0)) 
			(car exp))
			'((RTN))
			'((FEXIT))
		)
	)
)

;on compile les labels
(defun compilLabels (exp env fenv nomf)
	(let	(
			(nivEmp (assoc nomf fenv)) (corps (gensym "CORPS"))
		)
		(append
			`((JMP (@ ,corps)))
			(compilFctAux (car exp) env (fctEnv (car exp) fenv (+ 1 (cadr nivEmp)) ) nomf)
			`((@ ,corps))
			(compiler (cadr exp) env (fctEnv (car exp) fenv (+ 1 (cadr nivEmp)) ) nomf) 
		)
	)
)

;on compile les fonctions auxiliaire du labels (fonction utilisee par compilLabels)
(defun compilFctAux (exp env fenv nomf)
	(if (null exp ) ()
		(let	(
				(nivEmp (assoc (caar exp) fenv))
			)
			(append
				'((FENTRY))
				`((@ ,(caar exp)))
				(compiler (caddar exp)  (paramEnv (cadar exp) env 1 (cadr nivEmp) ) fenv (caar exp))
				'((RTN))
				'((FEXIT))
				(compilFctAux (cdr exp) env fenv nomf)
			)
		)
	)
)

;on augmente un environnement avec les parametres d'une fonction, passes en parametre
;on ajoute un couple (nomParam (LOC -deplacement niveauEmpilement) ) pour chaque parametre
(defun paramEnv (exp env dep nivEmp)   
	(if (atom exp) env
		(paramEnv (cdr exp) (cons (cons (car exp)`(LOC ,(- 0 dep) ,nivEmp)) env) (+ 1 dep) nivEmp)
	)
)


;on augmente un environnement fonctionnel avec les fonctions passees en parametre.
;on ajoute un couple (nomFonction niveauEmpilement) pour chaque fonction
(defun fctEnv (exp fenv nivEmp)
	(if (atom exp) fenv
		(fctEnv (cdr exp) (cons `(,(caar exp) ,nivEmp) fenv ) nivEmp)
	)
)

;on compile l'appel d'une fonction
;(f 1 2 3)
(defun compilAppel (exp env fenv nomf)
	(let	(
			(n (length (cdr exp))) (nivEmp (assoc (car exp) fenv))
		)
		(append 
			(compilParam (cdr exp)  env fenv nomf)
			`((PUSH (:DIESE ,n)))
			`((MOVE (:Rx :FP) (:Rx :R1)))
			`((MOVE (:Rx :SP) (:Rx :FP)))
			`((MOVE (:Rx :SP) (:Rx :R2)))
			`((SUB  (:DIESE ,n) (:Rx :R2)))
			`((SUB  (:DIESE 1) (:Rx :R2)))
			`((PUSH (:Rx :R2))) 
			`((PUSH (:Rx :R1)))
			(if nivEmp  `((PUSH (:DIESE ,(cadr nivEmp))))  `((PUSH (:DIESE ,0))))
			`((JSR (@ ,(car exp))))
		)
	)
)

;on compile et empile les parametres d'une fonction
(defun compilParam (exp  env fenv nomf)
	(if (atom exp) ()
		(append 
			(compiler (car exp) env fenv nomf)
			`((PUSH (:Rx :R0)))
			(compilParam (cdr exp) env fenv nomf)
		)
	)
)
