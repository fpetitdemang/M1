; FONCTIONS DE MANIPULATION DES PROTOTYPES
;
; un prototype est une structure de donnees qui representent une liste de parametres identique a celles
; qui sont passees a 'defun' ou 'defmacro'. Les fonctions qui suivent concernent toutes les prototypes.
; Elles servent a creer un prototype vide ou base sur une liste de parametres, a faire l'association entre
; les variables du prototype et une liste d'arguments,...
;
; structure : '(<liste variables classiques> <liste variables optionnelles> <liste variables par mots-cles>
; reste)'
;
; rem :
;	- le terme 'liste de variables' designe une liste composee aussi bien de variables
; que de couples '(variable valeur)'
;	- 'reste' peut etre nil, une variable ou un couple '(variable valeur)'


; fonctions auxiliaires pour la manipulation des prototypes

(mdefun empty-function-prototype()
  (list nil nil nil nil)
  )


; prototype : un prototype

; resultat : la liste des variables 'ordinaires'

(mdefun get-other(prototype)
  (car prototype)
  )


; prototype : un prototype

; resultat : la liste des variables optionnelles

(mdefun get-optional(prototype)
  (cadr prototype)
  )


; prototype : un prototype

; resultat : la liste des variables par mots-cles

(mdefun get-key(prototype)
  (caddr prototype)
  )


; prototype : un prototype

; resultat : le reste

(mdefun get-rest(prototype)
  (cadddr prototype)
  )


; prototype : un prototype
; x : une variable ou un couple '(variable valeur)'

; resultat : ajoute 'x' a liste des variables 'ordinaires'

(mdefun add-other(prototype x)
  (let* ((l (last (get-other prototype)))
	; sans 'copy-tree', problemes dus a la chirurgie
	(new-x (copy-tree x))
	(v (cond
	    ((is-variable new-x) new-x)

	    ((is-variable-value new-x) new-x)

	    (t (error "add-other : ~W n'est ni une variable, ni un couple (variable valeur)" x)))))
    
    (if l
	(setf (cdr l) (cons v nil))
      (setf (car prototype) (cons v nil))))
  prototype
  )


; prototype : un prototype
; x : une variable ou un couple '(variable valeur)'

; resultat : ajoute le couple '(x nil)' ou 'x' a liste des variables optionnelles

(mdefun add-optional(prototype x)
  (let* ((l (last (get-optional prototype)))
	 ; sans 'copy-tree', problemes dus a la chirurgie
	(new-x (copy-tree x))
	(v (cond
	    ((is-variable new-x) (list new-x nil))

	    ((is-variable-value new-x) new-x)

	    (t (error "add-optional : ~W n'est ni une variable, ni un couple (variable valeur)" x)))))

    (if l
	(setf (cdr l) (cons v nil))
      (setf (car (cdr prototype)) (cons v nil))))
  prototype
  )


; prototype : un prototype
; x : une variable ou un couple '(variable valeur)'

; resultat : ajoute le couple '(x nil)' ou 'x' a liste des variables par mots-cles

(mdefun add-key(prototype x)
  (let* ((l (last (get-key prototype)))
	  ; sans 'copy-tree', problemes dus a la chirurgie
	 (new-x (copy-tree x))
	 (v (cond
	     ((is-variable new-x) (list new-x nil))

	     ((is-variable-value new-x) new-x)
	    
	     (t (error "add-key : ~W n'est ni une variable, ni un couple (variable valeur)" x)))))
    
    (if l
	(setf (cdr l) (cons v nil))
      (setf (car (cddr prototype)) (cons v nil))))
  prototype
  )


; prototype : un prototype
; x : une variable ou un couple '(variable valeur)'

; resultat : definit 'x' comme le reste de prototype, en ecrasant le reste actuel

(mdefun add-rest(prototype x)
  (let* ((new-x (copy-tree x))
	 (v (cond
	     ((is-variable new-x) new-x)

	     ((is-variable-value new-x) new-x)

	     (t (error "add-rest : ~W n'est ni une variable, ni un couple (variable valeur)" x)))))

    (setf (car (cdddr prototype)) (copy-tree x)))
  prototype
  )


; prototype : un prototype
; x : une variable
; val : une expression quelconque

; resultat : si 'x' est une variable 'ordinaire' du prototype, associe 'val' a la variable 'x',
; sinon ne fait rien

(mdefun set-other-value(prototype x val)
  (if (is-variable x)
      (let ((where (find-variable x (get-other prototype))))
	(if where
	    (if (is-variable (car where))
		(setf (car where) (list x val))
	      (setf (car (cdar where)) val))))

    (error "set-other-value : ~W n'est pas une variable" x))
  
  prototype
  )


; prototype : un prototype
; x : une variable
; val : une expression quelconque

; resultat : si 'x' est une variable optionnelle du prototype, associe 'val' a la variable 'x',
; sinon ne fait rien

(mdefun set-optional-value(prototype x val)
  (if (is-variable x)
      (let ((where (find-variable x (get-optional prototype))))
	(if where
	    (setf (car (cdar where)) val)))

    (error "set-optional-value : ~W n'est pas une variable" x))

  prototype
  )


; prototype : un prototype
; x : une variable
; val : une expression quelconque

; resultat : si 'x' est une variable par mot-cle du prototype, associe 'val' a la variable 'x',
; sinon ne fait rien

(mdefun set-key-value(prototype x val)
  (if (is-variable x)
      (let ((where (find-variable x (get-key prototype))))
	(if where
	    (setf (car (cdar where)) val)))

    (error "set-key-value : ~W n'est pas une variable" x))

  prototype
  )


; prototype : un prototype
; val : une expression quelconque

; resultat : si le reste du prototype n'est pas nil, associe 'val' au reste, sinon ne fait rien

(mdefun set-rest-value(prototype val)
  (let ((where (cdddr prototype)))
    (if (and where
	     (car where))
	(if (is-variable (car where))
	    (setf (car where) (list (car where) val))
	  (setf (car (cdar where)) val))))
  )


;*********** A CONTINUER *************

; expr : une expression quelconque

; resultat : vrai si 'expr' est un prototype valide, faux sinon

(mdefun is-function-prototype(expr)
  (and (listp expr)
       (= (length expr) 4)
       (or (is-variable-list (car expr))
	   (is-variable-value-list (car expr)))
       (is-variable-value-list (cadr expr))
       (is-variable-value-list (caddr expr))
       (or (null (cadddr expr)) (is-variable (cadddr expr))))
  )


; lparam : une liste de parametres semblable a celles donnees a 'defun' ou 'defmacro'

; resultat : renvoie nil si 'lparam' n'est pas une liste de parametres valide, sinon renvoie
; le prototype correspondant a la liste de parametres

; rem : le code de la fonction depend helas de la representation effective des prototypes.
; Une amelioration notable consisterait a enlever cette dependance

(mdefun make-function-prototype(lparam)
  (let ((state-other 0)
	(state-optional 1)
	(state-key 2)
	(state-rest 3)
	(result (empty-function-prototype)))
    (labels
	((aux(lp state)
	     (if lp
		 (cond
		  ((= state state-other)
		   (case (car lp)
		     ('&optional (aux (cdr lp) state-optional))
		  
		     ('&key (aux (cdr lp) state-key))

		     ('&rest (aux (cdr lp) state-rest))

		     (t (if (is-variable (car lp))
			    (progn
			      (add-other result (car lp))
			      (aux (cdr lp) state))
			  nil))))

		  ((= state state-optional)
		   (case (car lp)
		     ('&optional nil)
		  
		     ('&key (aux (cdr lp) state-key))
		
		     ('rest (aux (cdr lp) state-key))

		     (t (if (or (is-variable (car lp))
				(is-variable-value (car lp)))
			    (progn
			      (add-optional result (car lp))
			      (aux (cdr lp) state))
			  nil))))

		  ((= state state-key)
		   (case (car lp)
		     ('&optional nil)
		     
		     ('&key nil)
		     
		     ('&rest (aux (cdr lp) state-rest))
		     
		     (t (if (or (is-variable (car lp))
				(is-variable-value (car lp)))
			    (progn
			      (add-key result (car lp))
			      (aux (cdr lp) state))
			  nil))))
		  
		  ((= state state-rest)
		   (case (car lp)
		     ('&optional nil)
		     
		     ('&key nil)
		     
		     ('&rest nil)
		     
		     (t (if (cdr lp)
			    nil
			  (add-rest result (car lp)))))))
	       t)))

      (if (aux lparam state-other)
	  result
	nil)))
  )


; prototype : un prototype
; largs : une liste d'arguments

; resultat : si le prototype et la liste d'arguments sont incompatibles, renvoie nil, sinon
; associe a chaque variable du prototype la valeur correspondante dans la liste d'arguments

; rem : le code de la fonction depend helas de la representation effective des prototypes.
; Une amelioration notable consisterait a enlever cette dependance

; exemple :	prototype = '((x y) () ((z 1) (w 0)) nil)'
;		largs  	  = '(2 3 :w 3)'
; 	 	-> prototype = '((x 2) (y 3) () ((z 1) (w 3)) nil)'

(mdefun assoc-prototype-arguments(prototype largs)
  (let ((state-other 0)
	(state-optional 1)
	(state-key 2)
	(state-rest 3))
    (labels
	; var : une variable
     	; keyw: un mot-cle

     	; resultat : renvoie vrai si le mot-cle 'keyw' correspond a la variable 'var'

	; exemple :	'(is-variable-keyword 'x :x)' -> t
     	;		'(is-variable-keyword 'x :a)' -> nil
	((is-variable-keyword(var keyw)
			     (and (is-variable var)
				  (keywordp keyw)
				  (string= (symbol-name var)
					   (symbol-name keyw))))

	; state : un etat qui indique quel type de variables (optionnel, par mot-cle,..)
	; on essaie d'associer a l'argument courant

	; resultat : renvoie l'etat suivant
	(step-prototype(state)
			(if (= state state-rest)
			    nil
			  (elt prototype (+ state 1))))


	; val : une expression quelconque
	; lc : une liste de couples '(variable valeur)'

	; resultat : si 'val' est un mot-cle correspondant a une variable de la liste,
	; renvoie la sous-liste maximal de 'lc' de premier element le couple '(variable valeur)', nil sinon

	; exemple :	val = ':b'
	;		lc  = '((a 1) (b 2) (c 3))'
	;		-> '((b 2) (c 3))'

	(find-keyword(val lc)
		     (if (atom lc)
			 nil
		       (if (is-variable-keyword (caar lc) val)
			   lc
			 (find-keyword val (cdr lc))))
		     )


	; lp : une liste de variables ou de couples '(variable valeur)'
	; lv : une liste d'arguments
	; state : indique a quel type de variables (optionnel,..) on associe les elements
	; de la liste d'arguments

	(main(lp lv state)
	     (if (null lp)
		 (if (not (= state state-rest))
		     (main (step-prototype state) lv (+ state 1))
		   (if lv
		       nil
		     t))

	       (cond
		((= state state-other)
		 (cond
		  (lv (if (is-variable (car lp))
			  (set-other-value prototype (car lp) (car lv))
			(set-other-value prototype (caar lp) (car lv)))
		      (main (cdr lp) (cdr lv) state))
		   (t nil)))
		
		((= state state-optional)
		 (if lv
		     (set-optional-value prototype (caar lp) (car lv)))
		 (main (cdr lp) (cdr lv) state))

		((= state state-key)
		 (if lv
		     (let ((couple (find-keyword (car lv) lp)))
		       (if (and couple
				(cdr lv))
			   (progn (set-key-value prototype (caar couple) (cadr lv))
				  (main lp (cddr lv) state))
			 (main nil lv state)))
		   
		   (main nil lv state)))

		((= state state-rest)
		 (set-rest-value prototype lv)
		 (main nil nil state-rest))))))
	
	(main (get-other prototype) largs state-other)))
  )


; prototype : un prototype

; resultat : renvoie vrai si le prototype ne contient aucune variable optionnel, par mot-cle
; ou de reste

(mdefun is-simple-prototype(prototype)
	(and (null (get-optional prototype))
	     (null (get-key prototype))
	     (null (get-rest prototype)))
	)


; prototype : un prototype

; resultat : la liste des variables du prototype dans l'ordre : variables 'ordinaires',
; variables optionnelles, variables par mots-cles, reste

; exemple :	prototype = '((x y) ((z nil)) ((a 0) (b 1)) l)'
;		 -> '(x y z a b l)'

(mdefun get-prototype-variables(prototype)
  (let ((rest (get-rest prototype)))
    (append (get-variables (get-other prototype))
	    (get-variables (get-optional prototype))
	    (get-variables (get-key prototype))
	    (if rest
		(get-variables (list rest))
	      rest)))
  )


; prototype : un prototype dont toutes les variables sont valuees i.e. qui a ete traite
; avec succes par 'assoc-prototype-arguments'

; resultat : la liste des valeurs associees aux variables du prototype dans l'ordre :
; variables 'ordinaires', variables optionnelles, variables par mots-cles, reste

; exemple : 	prototype = '(((x 1) (y 2)) ((z 3)) ((a 4) (b 5)) (l (6 7 8)))'
;		-> '(1 2 3 4 5 6 7 8)'

(mdefun get-prototype-values(prototype)
  (let ((rest (get-rest prototype)))
    (append (get-values (get-other prototype))
	    (get-values (get-optional prototype))
	    (get-values (get-key prototype))
	    (if rest
		`((list ,@(car (get-values (list rest)))))
	      rest)))
  )

