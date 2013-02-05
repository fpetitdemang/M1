; GESTION DE L'ENVIRONNEMENT DE COMPILATION
;
; Pour pouvoir compiler correctement (espoir !) le code LISP, le compilateur gere
; un environnement qui regroupe les variables et les fonctions dans la portee syntaxique du
; code courant. De plus, il gere une variable de statut qui contient, entre autres, les
; informations necessaires a la gestion de la recursivite terminale, et la gestion des appels de fonctions.
;
; L'environnement est constitue d'une liste de 2 elements : le premier pour les variables, le deuxieme pour
; les fonctions. Chaque environnement (variables et fonctions) est gere par niveaux.
;
; Pour les variables, chaque element constituant un niveau est une liste de couples '(variable decalage)'
; ou 'decalage' est la valeur a ajouter au regsitre FP (frame pointer) pour obtenir l'adresse sur la pile
; de la variable.
;
; exemple : '(x . -2)'		       PILE
;  |	adresses decroissantes	|	..	|	decalages croissants	|
;  |				|	3	| <-- x				|
;  |				|	7	|				|
;  V				|	0	| <-- FP      			V
;
;
; Pour les fonctions, chaque element constituant un niveau est une liste de quadruplets '(nom_fonction symbole_saut
; prototype macro)' ou 'symbole_saut' est le symbole qui sera genere par le compilateur pour reperer le debut
; de la fonction, 'prototype' est le prototype de la fonction (voir 'gen_proto.lisp') et 'macro' est un booleen
; qui indique si c'est une macro ou une fonction.
;
;
; exemple : '(defun foo(n m)
;		(let ((x (* n m))
;		      (y (+ n m)))
;
;		  (labels ((f1(n)
;				(* n n))
;			   (f2(n)
;				(* n n n)))

;			(- (f1 x) (f2 y)))))'
;
; Si le code courant est '(- (f1 x) (f2 y))', l'environnement est alors :
; '( (						LES VARIABLES
;	((Y . 6)				niveau 0
;	 (X . 5)
;	 (M . -1)
;	 (N . -2))
;    )
;
;    (						LES FONCTIONS
;	((F1 T1 ((N) NIL NIL NIL) NIL)		niveau 0
;	 (F2 T2 ((N) NIL NIL NIL) NIL))
;
;    	((FOO FOO ((N M) NIL NIL NIL) NIL))	niveau 1
;    )
;  )
;
; Si le code courant est '(* n n)', l'environnement est alors :
; '( (						LES VARIABLES
;	((N . -1))				niveau 0
;
;	((Y . 6)				niveau 1
;	 (X . 5)
;	 (M . -1)
;	 (N . -2))
;    )
;
;    (						LES FONCTIONS
;	((F1 T1 ((N) NIL NIL NIL) NIL)		niveau 0
;	 (F2 T2 ((N) NIL NIL NIL) NIL))
;
;   	((FOO FOO ((N M) NIL NIL NIL) NIL))	niveau 1
;    )
;  )
;
;
;
; Chaque appel de fonction genere un cadre de pile qui contient, entre autres, les parametres d'appel et les variables locales.
;
;  |	adresses decroissantes	    	PILE
;  |	    			| parametre 1		|	decalages croissants	|    FP = pointeur sur un cadre de pile
;  |				| parametre 2		|				|
;  V				|	...		|				|
;				| parametre n		|				V
;				| nombre de parametres	| <-- FP
;				| FP env englobant	|
;				| SP avant appel      	|
;				| FP appelant		|
;				| adresse de retour	|
;				| variable locale 1	|
;				|     	...		|
;				| variable locale n	|


; resultat : renvoie un environnement vide

(mdefun empty-env()
  (list nil nil)
  )


; env : un environnement de compilation

; resultat : renvoie l'environnement des variables

(mdefun variable-env(env)
  (car env)
  )


; env : un environnement de compilation

; resultat : renvoie l'environnement des fonctions

(mdefun function-env(env)
  (cadr env)
  )


; env : un environnement de compilation
; vars : une liste de variables
; base : un entier

; resultat : cree un nouveau niveau pour les variables de 'vars', en prenant 'base' comme base de calcul
; pour les decalages des variables et en les considerant comme des variables locales

; rem : 'base' sert a gerer l'imbrication des 'let'. Il doit correspondre au nombre de variables locales deja empilees
; dans le cadre de pile courant. Le nombre de variables locales empilees dans le cadre de pile
; courant est connu grace a la variable de statut de compilation qui contient une composante 'base' qui est mise a jour
; par le compilateur

(mdefun add-local-variables(env vars base)
  (add-current-local-variables `(,(cons nil (variable-env env))
				 ,(function-env env)) vars base)
  )


; env : un environnement de compilation
; vars : une liste de variables
; base : un entier

; resultat : ajoute les variables de 'vars' au niveau 0 de l'environnement des variables,
; en prenant 'base' comme base de calcul pour les decalages des variables et en les considerant
; comme des variables locales

; rem : 'base' sert a gerer l'imbrication des 'let'. Il doit correspondre au nombre de variables locales deja empilees
; dans le cadre de pile courant. Le nombre de variables locales empilees dans le cadre de pile
; courant est connu grace a la variable de statut de compilation qui contient une composante 'base' qui est mise a jour
; par le compilateur

(mdefun add-current-local-variables(env vars base)
   (labels
    ; lvars : une liste de variables
    ; offset : un decalage

    ; resultat : construit une liste de couples '(variable . offset)' ou 'offset' est le
    ; decalage de 'variable' par rapport a FP

    ((make-env(lvars offset)
	      (if (null lvars)
		  nil
		   
		`(,@(make-env (cdr lvars) (1+ offset))
		    (,(car lvars) . ,(+ base offset 4))))))

    (if (is-variable-list vars)
	(if (null (variable-env env))
	    `((,(make-env vars 1)) ,(function-env env))

	  `(((,@(make-env vars 1)
	       ,@(car (variable-env env)))
	     ,@(cdr (variable-env env)))
	    ,(function-env env)))

      (error "add-current-local-variables: liste de variables incorrecte ( recu: ~W )" vars)))
   )


; env : un environnement de compilation
; vars : une liste de variables

; resultat : cree un nouveau niveau pour les variables de 'vars', en les considerant comme des parametres
; pour le calcul des decalages

(mdefun add-param-variables(env vars)
  (add-current-param-variables `(,(cons nil (variable-env env))
				 ,(function-env env)) vars)
  )


; env : un environnement de compilation
; vars : une liste de variables

; resultat : ajoute les variables de 'vars' au niveau 0 de l'environnement des variables,
; en les considerant comme des variables locales pour le calcul des decalages

(mdefun add-current-param-variables(env vars)
  (labels
    ; lvars : une liste de variables
    ; offset : un decalage

    ; resultat : construit une liste de couples '(variable . offset)' ou 'offset' est le
    ; decalage de 'variable' par rapport a FP

   ((make-env(lvars offset)
	     (if (null lvars)
		 nil
		   
	       `(,@(make-env (cdr lvars) (1- offset))
		   (,(car lvars) . ,(- offset))))))

   (if (is-variable-list vars)
       (if (null (variable-env env))
	   `((,(make-env vars (length vars))) ,(function-env env))

	 `(((,@(make-env vars (length vars))
		 ,@(car (variable-env env)))
	    ,@(cdr (variable-env env)))
	   ,(function-env env)))
     
     (error "add-current-param-variables: liste de variables incorrecte ( recu: ~W )" vars)))
  )


; env : un environnement de compilation
; var : une variable

; resultat : renvoie le decalage associe a la variable si elle existe dans l'environnement, en cherchant par niveau
; croissant, nil si la variable n'existe pas dans l'environnement

(mdefun get-variable-offset(env var)
  (labels
   ; var-env : un environnement de variables

   ; resultat : le decalage associe a la variable 'var' ou nil si la variable n'existe pas dans
   ; l'environnement

   ((get(var-env)
	(if var-env
	    (or (cdr (assoc var (car var-env)))
		(get (cdr var-env)))
	  nil)
	))
   
   (if (is-variable var)
       (get (variable-env env))
     (error "get-var-offset: ~W n'est pas une variable" var)))
  )


; env : un environnement de compilation
; var : une variable

; resultat : renvoie le numero du premier niveau dans lequel on trouve la variable en cherchant
; par niveau croissant, nil si la variable n'existe pas dans l'environnement

(mdefun get-variable-level(env var)
  (labels
   ; env-var : un environnement de variables
   ; level-number : le numero du niveau qu'on explore

   ; resultat : le numero du premier niveau qui contient la variable ou nil si la variable n'existe pas
   ; dans l'environnement

   ((get(var-env level-number)
	(if var-env
	    (if (assoc var (car var-env))
		level-number
	      (get (cdr var-env) (+ level-number 1)))
	  nil)
	))
   
   (if (is-variable var)
       (get (variable-env env) 0)
     (error "get-var-level: ~W n'est pas une variable" var)))
  )


; env : un environnement de compilation
; lfunc: une liste de couples '(name prototype)' ou 'name' est un nom de fonction et 'prototype 'son prototype

; resultat : cree un nouveau niveau pour les fonctions de 'lfunc' dans l'environnement des fonctions

(mdefun add-functions(env lfunc)
  (add-current-functions (list (variable-env env) (cons nil (function-env env))) lfunc)
  )


; env : un environnement de compilation
; lfunc: une liste de couples '(name prototype)' ou 'name' est un nom de fonction et 'prototype 'son prototype

; resultat : ajoute les fonctions de 'lfunc' au niveau 0 de l'environnement des fonctions

(mdefun add-current-functions(env lfunc)
  (labels
   ; lfunc : une liste de couples '(name prototype)' ou 'name' est un nom de fonction et
   ; 'prototype' son prototype

   ; resultat : renvoie t si une des fonctions de 'lfunc' est deja definie dans le niveau 0 de l'environnement
   ; des fonctions, nil sinon

   ((is-one-function-already-defined(lfunc)
				    (if lfunc
					(if (is-function-defined-1 env (car (car lfunc)))
					    t
					  (is-one-function-already-defined (cdr lfunc)))

				      nil))
    
    ; lfunc : une liste de couples '(name prototype)' ou 'name' est un nom de fonction et
    ; 'prototype' son prototype

    ; resultat : renvoie une liste qui est au meme format qu'un niveau de l'environnement
    ; des fonctions correspondant aux fonctions de 'lfunc'

    (make-env(lfunc)
		     (if lfunc
			 (cons `(,(caar lfunc) ,(gentemp) ,(cadar lfunc) nil)
			       (make-env (cdr lfunc)))
		       nil)))

   (cond
    ; verification du format de 'lfunc'
    ((not (is-function-declaration-list lfunc :use-prototype t))
     		(error "add-current-functions: ~W n'est pas une liste de declarations de fonctions valide" lfunc ))

    ; l'environnement des fonctions est vide
    ((null (function-env env))
     		`(,(variable-env env)
		  (,(make-env lfunc))))

    ; une des fonctions de 'lfunc' est deja definie au niveau 0 de
    ; l'environnement des fonctions
    ((is-one-function-already-defined lfunc)
     		(error "add-current-functions: une des fonctions est deja definie dans l'environnement courant ( recu: ~W )" lfunc))

    ; sinon
    (t 		`(,(variable-env env)
		  ((,@(make-env lfunc)
		      ,@(car (function-env env)))
		   ,@(cdr (function-env env)))))))
  )


; env : un environnement de compilation
; func : un couple '(name prototype)' ou 'name' est le nom d'une fonction et 'prototype'
; son prototype
; sym : un symbole qui est utilise par le compilateur pour reperer le debut de la fonction
; is-macro : indique si c'est un macro ou une fonction

; resultat : cree un nouveau niveau pour la fonction 'func' dans l'environnement des fonctions

(mdefun add-one-function(env func sym &key (is-macro nil))
  (add-current-one-function `(,(variable-env env)
			      (nil ,@(function-env env)))
			    func
			    sym
			    :is-macro is-macro)
  )


; env : un environnement de compilation
; func : un couple '(name prototype)' ou 'name' est le nom d'une fonction et 'prototype'
; son prototype
; sym : un symbole qui est utilise par le compilateur pour reperer le debut de la fonction
; is-macro : indique si c'est un macro ou une fonction

; resultat : ajoute la fonction 'func' au niveau 0 de l'environnement des fonctions

(mdefun add-current-one-function(env func sym &key (is-macro nil))
  (cond
   ; verification du format de 'func'
   ((not (is-function-declaration func))
    		(error "add-current-one-function: ~W n'est pas une declaration de fonction valide" func))

   ; verification du type de 'sym'
   ((not (symbolp sym))
		(error "add-current-one-function: ~W n'est pas un symbole" sym))
  
   ; l'environnement des fonctions est vide
   ((null (function-env env))
    		`(,(variable-env env)
		  (((,(car func) ,sym ,(cadr func) ,is-macro)))))

   ; il existe deja une fonction/macro de meme nom au niveau 0
   ((is-function-defined-1 env (car func) :include-macros t)
    		(error "add-current-functions: fonction/macro deja definie dans l'environnement courant ( recu: ~W )" func))

   ; sinon
   (t 		`(,(variable-env env)
		  (((,(car func) ,sym ,(cadr func) ,is-macro)
		    ,@(car (function-env env))) ,@(cdr (function-env env))))))
  )


; env : un environnement de compilation
; name : nom d'une fonction
; include-macros : indique si la fonction doit inclure les macros dans sa recherche
; macros-only : indique si la fonction doit considerer uniquement les macros

; resultat : recherche une fonction de nom 'name' par niveau croissant et renvoie le symbole
; de saut associe a la fonction, nil s'il n'y a pas de fonction 'name' dans l'environnement
; de fonctions

(mdefun get-function-symbol(env name &key (include-macros nil) (macros-only nil))
  (if (symbolp name)
      (let ((func (is-function-defined env name :include-macros include-macros :macros-only macros-only)))
	(if func
	    (cadr func)
	  nil))

    (error "get-function-symbol: ~W func n'est pas un nom de fonction" name))
  )


; env : un environnement de compilation
; name : nom d'une fonction
; include-macros : indique si la fonction doit inclure les macros dans sa recherche
; macros-only : indique si la fonction doit considerer uniquement les macros

; resultat : recherche une fonction de nom 'name' par niveau croissant et renvoie le prototype
; associe a la fonction, nil s'il n'y a pas de fonction 'name' dans l'environnement de fonctions

(mdefun get-function-prototype(env name &key (include-macros nil) (macros-only nil))
  (if (symbolp name)
      (let ((func (is-function-defined env name :include-macros include-macros :macros-only macros-only)))
	(if func
	    (caddr func)
	  nil))
    
    (error "get-function-prototype: ~W func n'est pas un nom de fonction" name))
  )


; env : un environnement de compilation
; name : nom d'une fonction
; include-macros : indique si la fonction doit inclure les macros dans sa recherche
; macros-only : indique si la fonction doit considerer uniquement les macros

; resultat : renvoie un quadruplet '(name symbole_saut prototype macro)' s'il existe une fonction/macro 'name'
; dans l'environnement des fonctions, nil sinon

(mdefun is-function-defined(env name &key (include-macros nil) (macros-only nil))
   (labels
    ; func-env : un environnement de fonctions

    ; resultat : renvoie un quadruplet '(name symbole_saut prototype macro)' s'il existe une fonction/macro 'name'
    ; dans l'environnement des fonctions, nil sinon

   ((function-defined(func-env)
		     (if func-env
			 (let ((func (assoc name (car func-env))))
			   (if func
			       (cond
				(macros-only (if (cadddr func)
						 func
					       nil))			
		       
				(include-macros func)
		       
				(t (if (cadddr func)
				       nil
				     func)))
			     
			     (function-defined (cdr func-env))))
		       
		       nil)))
   
   (if (symbolp name)
       (function-defined (function-env env))
     
     (error "is-function-defined: ~W func n'est pas un nom de fonction" name)))
   )


; env : un environnement de compilation
; name : nom d'une fonction
; include-macros : indique si la fonction doit inclure les macros dans sa recherche
; macros-only : indique si la fonction doit considerer uniquement les macros

; resultat : renvoie un quadruplet '(name symbole_saut prototype macro)' s'il existe une fonction/macro 'name'
; au niveau 0 (au premier niveau) de l'environnement des fonctions, nil sinon

(mdefun is-function-defined-1(env name &key (include-macros nil) (macros-only nil))
  (let ((func (assoc name (car (function-env env)))))
    (if func
	(cond
	 (macros-only (if (cadddr func)
			  func
			nil))
	 
	 (include-macros func)
	 
	 (t (if (cadddr func)
		nil
	      func)))
      
      nil))
  )


; env : un environnement de compilation
; name : nom d'une fonction
; include-macros : indique si la fonction doit inclure les macros dans sa recherche
; macros-only : indique si la fonction doit considerer uniquement les macros

; resultat : renvoie le numero du premier niveau dans lequel on trouve une fonction 'name', nil
; s'il n'y a pas de fonction 'name' dans l'environnement des fonctions

(mdefun get-function-level(env name &key (include-macros nil) (macros-only nil))
  (labels
   ; func-env : un environnement de fonctions
   ; level : le numero du niveau qu'on explore

   ; resultat : renvoie le numero du premier niveau dans lequel on trouve une fonction 'name', nil
   ; s'il n'y a pas de fonction 'name' dans l'environnement des fonctions

   ((get-level(func-env level)
	      (if func-env
		  (if (is-defined (car func-env))
		      level
		    
		    (get-level (cdr func-env)
			       (+ level 1)))
		
		nil))


    ; env-level : une liste au meme format qu'un niveau d'un environnement de fonctions
    
    ; resultat : renvoie vrai s'il existe une fonction 'name' dans 'env-level', nil sinon

    (is-defined(env-level)
	       (let ((where (assoc name env-level)))
		 (if where
		     (cond
		      (macros-only (cadddr where))
		      
		      (include-macros where)
		      
		      (t (null (cadddr where))))
		   
		   nil))))
	  
   (get-level (function-env env) 0))
  )


; env : un environnement de compilation
; status : une variable de statut de la compilation (voir plus bas)
; name : nom de la fonction de destination

; resultat : renvoie le nombre de niveaux qui separe le niveau dans lequel on se trouve
; et qui est indique par le champs 'current-env' de 'status', du premier niveau qui
; contient la fonction 'name', ou nil s'il n'y a pas de fonction 'name' dans l'environnement
; des fonctions ou si 'current-env' vaut nil i.e. si on ne se trouve pas dans une fonction

(mdefun get-env-offset(env status name)
  (let* ((current (get-current-env status))
	 (function-env (function-env env))
	 (dest (get-function-level env name)))

    (if (and current
	     (car current)
	     dest)
	(- (- (length function-env) (length current)) dest)
      
	nil))
  )


;**************************************************************************************************************
;**************************************************************************************************************

; GESTION DU STATUT DE LA COMPILATION
;
; Le statut de compilation est une liste de 4 elements :
; 	base : indique le nombre de variables locales empilees dans le cadre de pile courant; cela
; sert pour l'imbrication des 'let'
;
; 	current-env : pointe sur un niveau d'un environnement des fonctions; cela donne le niveau
; a partir duquel les fonctions sont connues
;
; 	in-label : indique si on se trouve dans le corps d'un 'labels'; utile pour savoir si on doit ajouter
; les fonctions locales dans le niveau 0 de l'environnement de fonctions courant, ou creer un nouveau niveau
;
;	terminal-recursion : vrai si un appel de fonction peut etre gere par ecrasement du cadre de pile
; de l'appelant


; base : un entier qui indique le nombre de variables locales empilees dans le cadre de pile courant
; current-env : pointe sur un niveau d'un environnement de fonctions
; in-label : un booleen qui indique si on se trouve dans le corps d'un 'labels'
; terminal-recursion : indique si on peut ecraser le cadre de pile de l'appelant pour generer un appel

; resultat : un statut de compilation

(mdefun make-status(&key (base 0) (current-env nil) (in-label nil) (terminal-recursion nil))
  (list base current-env in-label terminal-recursion)
  )


; status : un statut de compilation

; resultat : renvoie le champs 'base' de 'status'

(mdefun get-base(status)
  (car status)
  )


; status : un statut de compilation

; resultat : renvoie le champs 'current-env' de 'status'

(mdefun get-current-env(status)
  (cadr status)
  )


; status : un statut de compilation

; resultat : renvoie le champs 'in-label' de 'status'

(mdefun is-in-label(status)
  (caddr status)
  )


; status : un statut de compilation

; resultat : renvoie le champs 'terminal-recursion' de 'status'

(mdefun is-terminal-recursion(status)
  (cadddr status)
  )


; status : un statut de compilation
; base : un entier

; resultat : modifie le champs 'base' de 'status'

(mdefun set-base(status base)
  (setf (car status) base)
  status
  )


; status : un statut de compilation
; current-env : un pointeur sur un niveau d'un environnement de fonctions

; resultat : modifie le champs 'current-env' de 'status'

(mdefun set-current-env(status current-env)
  (setf (car (cdr status)) current-env)
  status
  )


; status : un statut de compilation
; in-label : un booleen

; resultat : modifie le champs 'in-label' de 'status'

(mdefun set-in-label(status in-label)
  (setf (car (cddr status)) in-label)
  status
  )


; status : un statut de compilation
; in-label : un booleen

; resultat : modifie le champs 'terminal-recursion' de 'status'

(mdefun set-terminal-recursion(status terminal-recursion)
  (setf (car (cdddr status)) terminal-recursion)
  status
  )


; status : un statut de compilation
; change : si 'change' est vrai, fait une copie de l'statut et modifie les champs suivant la valeur des
; autres parametres, sinon fait une copie pure et simple
; terminal-recursion : si 'terminal-recursion' est faux, il est mis a faux dans la copie; si il est vrai
; et si, 'override' est vrai ou 'terminal-recursion' est vrai dans 'status', alors il est mis a vrai
; dans la copie
; override : si 'override' est vrai, la valeur du champs 'terminal-recursion' de la copie ne depend
; pas de sa valeur dans 'status', sinon il en depend de la maniere indiquee pour 'terminal-recursion'

; resultat : renvoie une copie de 'status', eventuellement modifiee pour le champs 'terminal-recursion'

(mdefun copy-status(status &key (change nil) (terminal-recursion nil) (override t))
  (let ((new-status (copy-tree status)))
    (if change
	(if terminal-recursion
	    (if (or override
		    (is-terminal-recursion status))
		(set-terminal-recursion new-status terminal-recursion))
	  (set-terminal-recursion new-status terminal-recursion)))
    
    new-status)
  )



