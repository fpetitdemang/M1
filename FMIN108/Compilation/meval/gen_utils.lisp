; FONCTIONS OUTILS POUR LA COMPILATION
; rem :
;	- plusieurs fonctions ci-dessous ont des parametres 'env' et 'status'. 'env' contient
; un environnement comportant les variables et fonctions dans la portee lexicale de ou des
; expressions qui sont egalement passees a ces fonctions. Quant a 'status', elle contient divers
; informations qui permettent, entre autres, au compilateur de gerer la recursivite terminale
; (voir 'gen_env.lisp')
;
;	- un prototype est une structure de donnees qui represente une liste de parametres
; semblable a celles fournies a 'defun' (voir 'gen_proto.lisp')


(defmacro out(&rest l) `(format *standard-output* ,@l))


; ************************************************************
; ************************************************************

; outils pour la compilation des fonctions gerees directement par le compilateur

; resultat : renvoie la liste des fonctions ou formes speciales gerees directement par le compilateur

(mdefun get-builtin-functions()
  '(apply
    quote
    function
    defun
    defmacro
    defparameter
    labels
    setf
    setq
    progn
    let
    let*
    if
    while
    loop
    for
    cons
    car
    cdr
    list
    list*
    +
    1+
    -
    1-
    *
    /
    =
    <
    <=
    >
    >=
    not
    )
  )


; name : nom d'une fonction ou d'une forme speciale
; resultat : renvoie t si la fonction ou forme speciale de nom 'name' est geree directement par
; le compilateur

(mdefun is-builtin-function(name)
  (not (not (member name (get-builtin-functions))))
  )


; ************************************************************
; ************************************************************

; outils pour le traitement des macros, compilees ou non

; expr : une expression non atomique
; new-expr : l'expression de substitution

; resultat : remplace l'expression 'expr' par 'new-expr'

(mdefun displace(expr new-expr)
  (if (consp expr)
      (if (atom new-expr)
	  (setf (car expr) 'progn
		(cdr expr) (list new-expr))
    
	(setf (car expr) (car new-expr)
	      (cdr expr) (cdr new-expr)))

    (warn "displace: l'expression a remplace est atomique"))

  expr
  )


; expr : une expression quelconque
; env : l'environnement de l'expression
; status : statut de l'expression

; resultat : si l'expression 'expr' est un appel a une macro compilee, expanse la macro en faisant
; appel a la MACHINE VIRTUELLE. Cela suppose que la macro compilee a prealablement ete chargee
; dans la MACHINE VIRTUELLE

(mdefun expand-compiled-macro(expr env status)
	(if (consp expr)
	    (if (get-compiled-function (car expr) :macros-only t)
		;  'lande-code-appel' est une fonction de la MACHINE VIRTUELLE
		(lance-code-appel (compile-call expr env status) nil)
	      expr)
	  expr)
	)


; expr : une expression quelconque
; env : l'environnement de l'expression
; status : statut de l'expression

; resultat : expanse un appel a une macro, compilee ,ou non. Si l'expression 'expr'
; est un appel a une macro compilee, expanse la macro en faisant appel a la MACHINE VIRTUELLE.
; Cela suppose que la macro compilee a prealablement ete chargee dans la MACHINE VIRTUELLE.
; Sinon, si 'expr' est un appel a une macro classique, expanse la macro via 'macroexpand-1'

(mdefun expand-macro(expr env status)
	(if (consp expr)
	    (cond
	     ((get-compiled-function (car expr) :macros-only t)
	      		(lance-code-appel (compile-call expr env status) nil))

	     ((macro-function (car expr))
	      		(macroexpand-1 expr))
	     
	     (t expr))

	  expr)
	)

; ************************************************************
; ************************************************************

; expr : une expression quelconque

; resultat : renvoie vrai si 'expr' est une lambda-expression, faux sinon

(mdefun is-lambda-expression(expr)
  (and (consp expr)
       (eq (car expr) 'lambda)
       (>= (length expr) 3)
       (make-function-prototype (cadr expr))))


; ************************************************************
; ************************************************************

; fonctions de gestion des 'car' et 'cdr' emboites (caar, cadr,..)


; expr : une expression quelconque

; resultat : vrai si 'expr' est de la forme cadr, cddr.., faux sinon

(mdefun is-car-cdr-combination(expr)
  (labels
      ((ad-sequence(s i last)
	   (if (> i last)
	       t
	     (if (or (eq (elt s i) #\A)
		     (eq (elt s i) #\D))
		 (ad-sequence s (+ i 1) last)

	       nil))))
    
    (if (symbolp expr)
	(let* ((name (symbol-name expr))
	      (length (length name)))
	  (if (>= length 4)
	      (and (eq (elt name 0) #\C)
		   (eq (elt name (- length 1)) #\R)
		   (ad-sequence name 1 (- length 2)))

	    nil))
      
      nil))
  )


; expr : une expression quelconque

; resultat : si l'expression est de la forme '(cadr ..)' ou '(cddr ..)'.., renvoie
; une une expression de la forme '(car (cdr ..))' ou '(cdr (cdr ..))'.. ; sinon renvoie 'expr'

(mdefun simplify-car-cdr-combination(expr)
  (labels
   	; s	: une chaine de 'a' et de 'd'
   	; arg	: l'argument de l'expression de depart
      ((simplify(s arg)
		   (if (string= s "")
		       arg

		     (if (eq (elt s 0) #\A)
			 `(car ,(simplify (subseq s 1) arg))

		       `(cdr ,(simplify (subseq s 1) arg))))))
      
      (if (and (consp expr)
	       (is-car-cdr-combination (car expr))
	       (= (length expr) 2))
	  
	  (let ((operator (symbol-name (car expr))))
	    (simplify (subseq operator 1 (- (length operator) 1))
		      (cadr expr)))
      
	expr))
  )


; ************************************************************
; ************************************************************

; compilation d'une sequence d'instructions


; lexpr : une liste d'expressions
; env : environnement de la sequence d'expressions
; status : statut de la sequence d'expressions

; resultat : du code en pseudo-assembleur correspondant a la sequence d'expressions

(mdefun multi-compile(lexpr env status)
  (cond
   ((atom lexpr)
    		nil)
   
   ((null (cdr lexpr))
    		(compile-main (car lexpr) env status))
		       
   (t (let ((non-terminal-status (copy-status status :change t :terminal-recursion nil)))
	`(,@(compile-main (car lexpr) env non-terminal-status)
	    ,@(multi-compile (cdr lexpr) env status)))))
  )


; expr : une expression de la forme '(list* e1..en)'

; resultat : l'expression equivalente sous la forme '(cons e1 (cons e2..(cons en-1 en)..))'

(mdefun list*-to-cons(expr)
  (labels ((convert(l)
		   (if (cdr l)
		       `(cons ,(car l)
			      ,(convert (cdr l)))
		     (car l))))
	  
	  (if (and (listp expr)
		   (>= (length expr) 2)
		   (eq (car expr) 'list*))
	      (convert (cdr expr))
	    
	    (error "list*-to-cons: ~W invalide" l)))
  )


; ************************************************************
; ************************************************************

; divers fonctions pour la gestion des environnements de variables du COMPILATEUR, et ceux
; sous la forme '((var1 val1) var2 (var3)..)', comme celui du 'let' par exemple


; expr : une expression quelconque

; resultat : vrai si 'expr' est une variable, faux sinon

(mdefun is-variable(expr)
  (and (symbolp expr) (not (constantp expr)))
  )


; expr : une expression quelconque

; resultat : vrai si 'expr' est une liste de variables, faux sinon

(mdefun is-variable-list(expr)
  (labels ((is-var-list(l)
		       (if l
			   (and (is-variable (car l))
				(is-var-list (cdr l)))
			 t)))
	  
	  (if (listp expr)
	      (is-var-list expr)
	    nil))
  )


; expr : une expression quelconque

; resultat : vrai si 'expr' est de la forme '(variable valeur)' ou '(variable)', faux sinon

(mdefun is-variable-value(expr)
  (if (and (consp expr)
	   (null (cddr expr))
	   (is-variable (car expr)))
      t

    nil)
  )


; expr : une expression quelconque

; resultat : vrai si 'expr' est de la forme '((var1 val1) (var2)..)', faux sinon

(mdefun is-variable-value-list(expr)
  (labels ((is-var-val-list(l)
			   
			   (if (consp l)
			       (and (is-variable-value (car l))
				    (is-var-val-list (cdr l)))
			     t)))

    (if (listp expr)
	(is-var-val-list expr)
      nil))
  )


; lvar : une expression de la forme '((var1 val1) (var2) var3..)'

; resultat : renvoie la liste des variables de 'lvar' 

; exemple : '(a (b 1) (c))' -> '(a b c)'

(mdefun get-variables(lvar)
  (labels
   ((get-vars(lv)
	     (if lv
		 (cond
		  ((is-variable (car lv))
		   	(cons (car lv) (get-vars (cdr lv))))
			 
		  ((is-variable-value (car lv))
		   	(cons (caar lv) (get-vars (cdr lv))))
		  
		  (t (error "get-variables: liste de declaration de variables locales incorrecte (recu: ~W)" lvar))))))
   
   (get-vars lvar))
  )


; lvar : une expression de la forme '((var1 val1) (var2) var3..)'

; resultat : renvoie la liste des valeurs de 'lvar'

; exemple : '(a (b 1) c)' -> '(nil 1 nil)'

(mdefun get-values(lvar)
  (labels
   ((get-vals(l)
	     (if l
		 (cond
		  ((is-variable (car l)) 
		   	(cons nil (get-vals (cdr l))))

		  ((is-variable-value (car l))
		   	(cons (cadar l) (get-vals (cdr l))))

		  (t (error "get-values: liste de declaration de variables locales incorrecte (recu: ~W)" lvar))))))
   
   (get-vals lvar))
  )


; x : une variable
; lvar : une liste de variables ou de couples '(variable valeur)'

; resultat : renvoie la sous-liste maximale commencant par x ou le couple '(x valeur)'

(mdefun find-variable(x lvar)
  (labels ((find(lvar)
		(if (atom lvar)
		    nil

		  (if (is-variable (car lvar))
		      (if (equal x (car lvar))
			  lvar
			(find-variable x (cdr lvar)))
      
		    (if (equal x (caar lvar))
			lvar
		      (find-variable x (cdr lvar)))))))

	  (if (is-variable x)
	      (find lvar)
	    nil))
  )


; ************************************************************
; ************************************************************

; divers  fonctions pour la gestion des environnements de fonctions du COMPILATEUR
; (voir 'gen_env.lisp')


; expr : une expression quelconque

; resultat : vrai si 'expr' est une declaration de fonction, i.e. une liste
; de la forme '(name prototype)' ou 'name' est le nom d'une fonction
; i.e. un symbole, et 'prototype' est un prototype valide, ou une liste de parametres correctes
; semblable a celles fournies a 'defun', si 'use-prototype' est vrai; renvoie faux sinon

(mdefun is-function-declaration(expr &key (use-prototype t))
  (and (listp expr)
       (= (length expr) 2)
       (symbolp (car expr))
       (if use-prototype
	   (is-function-prototype (cadr expr))
	 (make-function-prototype (cadr expr))))
  )


; expr : une expression quelconque

; resultat : vrai si 'expr' est une liste de declarations de fonctions
; (voir 'is-function-declaration'), faux sinon

(mdefun is-function-declaration-list(expr &key (use-prototype t))
  (if (listp expr)
      (cond
       ((null expr) t)

       ((is-function-declaration (car expr) :use-prototype use-prototype) (is-function-declaration-list (cdr expr) :use-prototype use-prototype))

       ( nil))

    nil)
  )
