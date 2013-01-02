; ******************** METAEVAL: m�ta-�valuateur LISP ********************
; ********** r�alis� par Xavier ANDREANI **********

; ***** diff�rences avec l'�valuateur CLISP *****
;- l'environnement est un SCHEME-like (la distinction entre les variables introduites par setf et par defvar ou defparameter n'est pas g�r�e, car CLISP)
;- gestion "simplifi�e" des erreurs - la forme pass�e est suppos�e correcte
;- modifications pour g�rer les closures! on �vite le renvoie d'une closure car dans ce cas, on ne peut r�cup�rer le code 
;(function f) = #'f = 'f -> f
;(function (lambda ...)) = #'(lambda ...) = (lambda ...) -> (lambda ...) (modifi�e)
;- la capture d'environnement ne g�re que les variables locales et pas les fonctions locales! de part l'impl�mentation de l'environnement, on a des fonctions globales et locales. si on capture les fonctions locales  pour un #', on le fera aussi lors d'un defun et sutout d'un labels. mais pour le labels, on ne pourra utiliser de fonctions r�cursives. la fonction locale est masqu�e (car elle est locale) lors de l'appel � une fonction nomm�e. et elle ne peut etre captur�e � sa d�finition puisqu'elle n'existe pas encore. et meme si on la capturait, on serait oblig� d'utiliser une liste circulaire l=(nom (valeur corps) (l ...)), ce qui n'est pas g�r� par CLISP

; *** la fonction principale qui est sans cesse r�utilis�e pour la r�cursivit� ***

(mdefun meval (expr &optional (ensvar ensvar) (ensfct ensfct))
	; * si l'argument pass� est une liste
	(if (consp expr)
	    (cond
	     ; * si la t�te de liste est une macro CLISP dont la macro-expansion peut produire une liste contenant des fonctions ou macros qui ne sont pas g�r�es (fonctions ou macros de bas niveau commen�ant le plus souvent par SYSTEM::)
	     ((eq (car expr) 'loop) (metaevalloop (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'defmacro) (metaevaldefmacro (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'defun) (metaevaldefun (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'defparameter) (metaevaldefparameter (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'defconstant) (metaevaldefconstant (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'defvar) (metaevaldefvar (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'setf) (metaevalsetf (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'setq) (metaevalsetq (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'lambda) (metaevallambda (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'time) (metaevaltime (cdr expr) ensvar ensfct))
	     ; * si la t�te de liste est une macro CLISP
	     ((and (symbolp (car expr)) (macro-function (car expr))) (metamacro expr ensvar ensfct))
	     ; * si la t�te de liste est une forme sp�ciale CLISP
	     ((and (symbolp (car expr)) (special-form-p (car expr))) (metaevalspecialform expr ensvar ensfct))
	     ; * si la t�te de liste est une fonction ou macro CLISP dont le fonctionnement interne a besoin d'�tre modifi� pour �tre s�r que ce soit bien METAEVAL qui �value jusqu'au bout
	     ((eq (car expr) 'member) (metaevalmember (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'funcall) (metaevalfuncall (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'apply) (metaevalapply (cdr expr) ensvar ensfct ))
	     ((eq (car expr) 'mapcar) (metaevalmapcar (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'eval) (metaevaleval (cdr expr) ensvar ensfct))
	     ((eq (car expr) 'load) (metaevalload (cdr expr) ensvar ensfct))
	     ; * si la t�te de liste est une lambda-expression
	     ((and (consp (car expr)) (eq (caar expr) 'lambda)) (metaapplylambda expr ensvar ensfct ))
	     ; * si la t�te de liste est une macro METAEVAL
	     ((and (symbolp (car expr)) (macrovaleur (car expr) ensvar ensfct)) (metaapplymacro expr ensvar ensfct))
	     ; * si la t�te de liste est une fonction METAEVAL
	     ((and (symbolp (car expr)) (fctvaleur (car expr) ensvar ensfct )) (metaapplyfunction expr ensvar ensfct))
	     ; * cas par d�faut: on suppose que la t�te de liste est une fonction CLISP et on va �valuer tous ses arguments et si c'est faux, ben erreur!
	     (t (metafunction expr ensvar ensfct))
	     )
	  ; * si l'argument pass� n'est pas une liste, on lui cherche une valeur si c'est un symbole qui n'est pas un mot-clef en donnant la priorit� � l'environnement METAEVAL et si on ne trouve pas de valeur on va chercher dans l'environnement CLISP et si il n'y a toujours pas de valeur, ben erreur!
	  (if (keywordp expr)
	      expr
	    (if (symbolp expr)
		(metasymbol expr ensvar ensfct )
	      expr))))

; ***** traitement des formes sp�ciales CLISP *****

; *** identification de la forme sp�ciale ***
(mdefun metaevalspecialform (expr ensvar ensfct)
	(cond
	  ((eq (car expr) 'labels) (metaevallabels (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'function) (metaevalfunction (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'let*) (metaevallet* (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'let) (metaevallet (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'quote) (metaevalquote (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'if) (metaevalif (cdr expr) ensvar ensfct ))
	  ((eq (car expr) 'progn) (metaevalprogn (cdr expr) ensvar ensfct))))

; *** traitement de labels ***

(mdefun metaevallabels (expr ensvar ensfct)
	(labels ((aux (expr ensvar ensfct)
		      (if (atom (car expr))
			  (metaevalprogn (cdr expr) ensvar ensfct)
			(aux (cons (cdar expr) (cdr expr)) ensvar (makeenv (addproto (makefv (caaar expr) (cadaar expr) (cddaar expr) ensvar ensfct) (local ensfct)) (global ensfct))))))
	  (aux (cons (reverse (car expr)) (cdr expr)) ensvar ensfct)))
		 
; *** traitement de function (pas m�ta-d�finie car doit �valuer le function de CLISP) ***

(mdefun metaevalfunction (expr ensvar ensfct)
	(if (and (consp (car expr)) (equal 'lambda (caar expr)))
	    `(lambda ,(cadar expr) (let ,(local ensvar) ,(caddar expr)))
	    (car expr)))

; *** traitement de let* ***

(mdefun metaevallet* (expr ensvar ensfct )
	(if (atom (car expr))
	     (metaevalprogn (cdr expr) ensvar ensfct)
	   (if (atom (caar expr))
	       (metaevallet* (cons (cdar expr) (cdr expr)) (makeenv (cons (makevv (caar expr) nil) (local ensvar)) (global ensvar)) ensfct)
	     (metaevallet* (cons (cdar expr) (cdr expr)) (makeenv (addproto (makevv (caaar expr) (meval (cadaar expr) ensvar ensfct)) (local ensvar)) (global ensvar)) ensfct))))

; *** traitement de let *** A ENCAPSULER!!!

(mdefun metaevallet (expr ensvar ensfct )
	(metaevalprogn (cdr expr) (list (unionenv (reverse (varassoc (car expr) ensvar ensfct)) (local ensvar)) (global ensvar)) ensfct))

; *** traitement de quote ***

(mdefun metaevalquote (expr ensvar ensfct )
	(car expr))

; *** traitement de if ***

(mdefun metaevalif (expr ensvar ensfct )
	(if (meval (car expr) ensvar ensfct)
	    (meval (cadr expr) ensvar ensfct)
	  (meval (caddr expr) ensvar ensfct )))

; *** traitement de progn ***

(mdefun metaevalprogn (expr ensvar ensfct)
	(if (null (cdr expr))
	     (meval (car expr) ensvar ensfct)
	   (progn (meval (car expr) ensvar ensfct)
		  (metaevalprogn (cdr expr) ensvar ensfct))))

; ***** traitement des macros CLISP dont la macro-expansion peut produire des fonctions de bas niveau non g�r�es par METAEVAL (fonctions commen�ant par SYSTEM:: le plus souvent) *****

; *** traitement de time ***
(mdefun metaevaltime (expr ensvar ensfct)
	(eval (list 'time (list 'meval (list 'quote (car expr)) (list 'quote ensvar) (list 'quote ensfct)))))

; *** traitement de loop ***

(mdefun metaevalloop (expr ensvar ensfct)
	(if (meval (cadr expr) ensvar ensfct)
	    (progn
	      (metaevalprogn (cdddr expr) ensvar ensfct)
	      (metaevalloop expr ensvar ensfct))
	  nil))

; *** traitement de defmacro ***

(mdefun metaevaldefmacro (expr ensvar ensfct)
	(prog1
	    (eval (append (list 'defmacro (car expr) (cadr expr)) (cddr expr)))
	  (setmacro (car expr) (cadr expr) (cddr expr) ensvar ensfct)))

; *** traitement de defun ***

(mdefun metaevaldefun (expr ensvar ensfct)
	(prog1
	    (eval (append (list 'defun (car expr) (cadr expr)) (cddr expr)))
	  (setglobalfunction (car expr) (cadr expr) (cddr expr) ensvar ensfct)))

; *** traitement de defparameter ***

(mdefun metaevaldefparameter (expr ensvar ensfct)
	(let ((r (cstvaleur (car expr) ensvar ensfct))
	      (u (meval (cadr expr) ensvar ensfct)))
	  (if r
	      (error "METAEVAL: Constante ne peut etre modifi�e")
	    (prog1 (eval (list 'defparameter (car expr) (list 'quote u)))
	      (setglobalvaleur (car expr) u ensvar ensfct)))))

; *** traitement de defconstant ***

(mdefun metaevaldefconstant (expr ensvar ensfct)
	(let ((v (meval (cadr expr) ensvar ensfct)))
	  (prog1 (eval (list 'defconstant (car expr) (list 'quote v)))
	    (setconstant (car expr) v ensvar ensfct))))

; *** traitement de defvar ***

(mdefun metaevaldefvar (expr ensvar ensfct)
	(let ((r (cstvaleur (car expr) ensvar ensfct))
	      (u (meval (cadr expr) ensvar ensfct))
	      (v (varvaleur (car expr) ensvar ensfct)))
	  (if r
	      (error "METAEVAL: Constante ne peut etre modifi�e")
	    (if (not v)
		(prog1 (eval (list 'defvar (car expr) (list 'quote u)))
		  (setglobalvaleur (car expr) u ensvar ensfct))
	      (prog1 (eval (list 'defvar (car expr) (vval v)))
		(setglobalvaleur (car expr) (vval v) ensvar ensfct))))))

; *** traitement de setf ***

(mdefun metaevalsetf (expr ensvar ensfct)
  (if (= (length expr) 1)
      (error "METAEVAL: Mauvais nombre d'arguments pour setf")
    (if (= (length expr) 0)
	nil
      (let (r)
	(if (symbolp (car expr))
	    (setf r (metaevalsetq (list (car expr) (cadr expr)) ensvar ensfct))
	  (progn
	    (setf r (meval (cadr expr) ensvar ensfct))
	    (if (consp (car expr))
		(eval (list 'setf (cons (caar expr) (metaquoteargs (cdar expr) ensvar ensfct)) (list 'quote r)))
	      (error "METAEVAL: l'argument n'est pas modifiable par setf"))))
	(if (= (length expr) 2)
	    r
	  (metaevalsetf (cddr expr) ensvar ensfct))))))

; *** traitement de setq ***

(mdefun metaevalsetq (expr ensvar ensfct)
    (if (= (length expr) 1)
      (error "METAEVAL: Mauvais nombre d'arguments pour setq")
      (if (= (length expr) 0)
	  nil
	(let ((r (meval (cadr expr) ensvar ensfct)))
	  (if (symbolp (car expr))
	      (prog1 
		  (eval (list 'setq (car expr) (list 'quote r)))
		(setlocalvaleur (car expr) r ensvar ensfct))
	    (error "METAEVAL: l'argument n'est pas modifiable par setq"))
	  (if (= (length expr) 2)
	      r
	    (metaevalsetq (cddr expr) ensvar ensfct))))))

; ***** traitement des fonctions CLISP dont le fonctionnement a besoin d'�tre d�tourn� pour que l'�valuation se fasse bien jusqu'au bout par METAEVAL *****

; *** traitement de apply (pas m�ta-d�finie car doit faire un appel � l'EVAL de CLISP dans un cas) ***
;pourquoi traiter apply?
;pour que la m�ta-�valuation se fasse bien jusqu'au bout (jusqu'�-ce que ce soit termin� ou jusqu'�-ce qu'on ait plus le choix et que l'on doive appeller EVAL)
;si apply �tait g�r�e comme une fonction normale, METAEVAL �valuerait tous ses arguments
;si par exemple le premier argument est quelque chose du genre #'f, son �valuation produirait une closure
;on ne pourrait alors plus r�cup�rer le code de la fonction f et on aurait plus de choix autre que de faire EVAL
;c'est pareil dans le cas d'une lambda-expression
;donc, je continue la m�ta-�valuation en repassant � METAEVAL la fonction et ses arguments lorsque c'est possible

(mdefun metaevalmember (expr ensvar ensfct)
	(labels ((aux (a l r)
		      (if l
			  (if (metaevalapply `((quote ,r) (quote (,a ,(car l)))) ensvar ensfct)
			      l
			    (aux a (cdr l) r))
			nil)))
	  (if (cddr expr)
	      (aux (meval (car expr) ensvar ensfct) (meval (cadr expr) ensvar ensfct) (meval (cadddr expr) ensvar ensfct))
	    (aux (meval (car expr) ensvar ensfct) (meval (cadr expr) ensvar ensfct) 'eq))))

(mdefun metaevalfuncall (expr ensvar ensfct)
	(metaevalapply (append expr '(())) ensvar ensfct))

(mdefun metaevalapply (expr ensvar ensfct )
  (labels 
    ; * renvoie la liste de tous les arguments de la fonction pass�e � apply
    ((args (expr)
	  (if (null expr)
	      (error "METAEVAL: Pas assez d'arguments pour apply")
	    (if (null (cdr expr))
		(if (listp (meval (car expr) ensvar ensfct))
		    (quotelst (meval (car expr) ensvar ensfct))
		  (error "METAEVAL: Pas assez d'arguments pour apply"))
	      (cons (car expr) (args (cdr expr)))))))
    (meval (cons (meval (car expr) ensvar ensfct) (metaquoteargs (args (cdr expr)) ensvar ensfct)) ensvar ensfct)))

; *** traitement de mapcar ***
;il s'agit du m�me probl�me qu'avec apply
;je traite donc cette fonction en la faisant appeller "avantageusement" apply

(mdefun metaevalmapcar (expr ensvar ensfct)
	(napply (car expr) (meval (cadr expr) ensvar ensfct)))

; *** traitement de eval ***
;quand on tombe sur eval, on appelle METAEVAL

(mdefun metaevaleval (expr ensvar ensfct )
	(meval (meval (car expr) ensvar ensfct ) ensvar ensfct))

; *** traitement de load ***
; on m�ta-�value le contenu d'un fichier

(mdefun metaevalload (expr ensvar ensfct)
	(labels ((aux (X)
		      (let ((r (read X)))
			(if r
			  (progn (meval r ensvar ensfct) (aux X))
			  t))))
	  (let ((X (open (car expr))))
	    (aux X))))

; ***** traitement d'une lambda-expression *****
;on �value le corps de la lambda dans un environnement local que l'on cr�e gr�ce � let et o� l'on associe les param�tres � leurs valeurs 

(mdefun metaapplylambda (expr ensvar ensfct)
	(metaevallet (cons (quotelet (fctargs (cadar expr) (quotelst (metaargs (cdr expr) ensvar ensfct)) ensvar ensfct)) (cddar expr)) ensvar ensfct))

; ***** traitement de lambda (pas m�ta-d�finie car doit faire appel au EVAL de CLISP) *****

(mdefun metaevallambda (expr ensvar ensfct)
  (metaevalfunction expr ensvar ensfct))

; ***** traitement des macros METAEVAL *****
; d'abord ex�cution du corps de la macro dans l'environnement o� l'on a li� les param�tres � leurs valeurs non �valu�es / macro-expansion
;puis ex�cution de la valeur retourn�e

(mdefun metaapplymacro (expr ensvar ensfct)
	(let ((r (macrovaleur (car expr) ensvar ensfct)))
	  (if (fcompiledp r)
	      (vm-apply (car expr) (cdr expr))
	    (meval (metaevallet (cons (quotelet (fctargs (fargs r) (quotelst (cdr expr)) ensvar ensfct)) (fcorps r)) ensvar ensfct) (makeenv (fensvar r) (global ensvar)) ensfct))))

; ***** traitement des fonctions METAEVAL *****
;on restaure l'environnement local dans lequel s'est effectu�e la d�finition de la fonction
;on r�appelle METAEVAL en substituant la fonction par une lambda-expression

(mdefun metaapplyfunction (expr ensvar ensfct)
	(let ((r (fctvaleur (car expr) ensvar ensfct)))
	  (if (fcompiledp r)
	      (vm-apply (car expr) (metaquoteargs (cdr expr) ensvar ensfct))
	    (metaapplylambda (cons (append (list 'lambda (fargs r)) (fcorps r)) (quotelst (metaargs (cdr expr) ensvar ensfct))) (makeenv (fensvar r) (global ensvar)) ensfct))))


; ***** traitement des macros CLISP *****

(mdefun metamacro (expr ensvar ensfct)
	(meval (macroexpand-1 expr) ensvar ensfct))

; ***** traitement des fonctions CLISP *****

(mdefun metafunction (expr ensvar ensfct)
    (eval `(,(car expr) ,@(quotelst (metaargs (cdr expr) ensvar ensfct)))))

; * renvoie la valeur d'un symbole en cherchant d'abord dans l'environnement METAEVAL et ensuite dans CLISP (pas m�ta-d�finie car fait EVAL dans environnement CLISP

(mdefun metasymbol (expr ensvar ensfct )
	(let ((r (cstvaleur expr ensvar ensfct))
	      (v (varvaleur expr ensvar ensfct)))
	  (if r
	      (vval r)
	    (if v
		(vval v)
	      (eval expr)))))
	
; ***** fonctions utilitaires utilis�es par les fonctions ci-dessus *****

; * quote toutes les valeurs de la liste d'association car les arguments ne doivent pas �tre �valu�s

(mdefun quotelet (lst)
	(if (consp lst)
	    (cons (if (consp (car lst))
		      (list (caar lst) (list 'quote (cadar lst)))
		    (car lst))
		  (quotelet (cdr lst)))))

; * �value tous les arguments de la liste

(mdefun metaargs (expr ensvar ensfct)
	(if (null expr)
	     nil
	   (cons (meval (car expr) ensvar ensfct) (metaargs (cdr expr) ensvar ensfct))))

; * �value et quote tous les arguments de la liste

(mdefun metaquoteargs (expr ensvar ensfct)
	(if (null expr)
	     nil
	  (if (keywordp (car expr))
	      (cons (car expr) (metaquoteargs (cdr expr) ensvar ensfct))
	    (cons (list 'quote (meval (car expr) ensvar ensfct)) (metaquoteargs (cdr expr) ensvar ensfct)))))

; * prend une liste et retourne la m�me liste avec tous ses arguments quot�s
;le dernier argument de apply doit �tre une liste d'arguments
;or, j'ai l'intention de faire �valuer � METAEVAL quelque chose comme (fct arg1 arg2 ... argn)
;mais les derniers arguments de apply �tant dans une liste, ils ne doivent pas �tre �valu�s (lors de l'�valuation de la fonction, tous les arguments seront �valu�s), d'o� l'int�r�t de les quoter!

(mdefun quotelst (lst)
	(if (consp lst)
	    (cons (list 'quote (car lst)) (quotelst (cdr lst)))))

; * applique la fonction � chaque �l�ment de la liste
(mdefun napply (fct ens)
	(if (null ens)
	     nil
	   (cons (meval (list 'apply fct (list 'quote (car ens)) nil) ensvar ensfct) (napply fct (cdr ens)))))

; * retourne les associations variables-valeurs des param�tres pass�s � une fonction ou une macro, et g�re les mots-clefs
;(&optional, &rest et &key uniquement)
;on suppose que l'expression est correcte (l'ordre de &optional, &rest et &key n'est pas test� au pr�alable, ni leur nombre d'arguments)
;de toutes fa�ons, les fonctions et macros sont aussi d�finies dans l'environnement CLISP et au pire il y aura erreur � ce moment-l� 

(mdefun fctargs (args params ensvar ensfct)
	(labels
	    ((optional (args params)		       
		       (if (null args)
			   (if (null params)
			       nil
			     (error "METAEVAL: Trop d'arguments"))
			 (case (car args)
			   ('&rest (rest (cdr args) params))
			   ('&key (append (keyinit (cdr args)) (key (cdr args) params)))
			   (t (if (consp (car args))
				  (if (null params)
				      (cons (list (caar args) (meval (cadar args) ensvar ensfct)) (optional (cdr args) nil))
				    (cons (list (caar args) (meval (car params) ensvar ensfct)) (optional (cdr args) (cdr params))))
				(if (null params)
				    (cons (list (car args) nil) (optional (cdr args) nil))
				  (cons (list (car args) (meval (car params) ensvar ensfct)) (optional (cdr args) (cdr params)))))))))
	     (keyinit (args)
		      (if (or (null args) (equal (car args) '&rest))
			  nil
			(cons (car args) (keyinit (cdr args)))))
	     (key (args params)
		  (if (null params)
		      nil
		    (if (null args)
			(error "METAEVAL: Trop d'arguments")
		      (if (null (cdr params))
			  (error "METAEVAL: Les mot-clefs doivent apparaitrent par paires")
			(let ((r (meval (car params) ensvar ensfct)))
			      (if (keywordp r)
				  (let ((v (validkey r args)))
				    (if v
					(cons (list v (meval (cadr params) ensvar ensfct)) (key args (cddr params)))
				      (error "METAEVAL: Mot-clef invalide")))))))))
	     (rest (args params)
		   (cons (list (car args) (metaargs params ensvar ensfct))
			 (append (keyinit (cdr args)) (key (cdr args) params)))))
	  (if (null args)
	      (if (null params)
		  nil
		(error "METAEVAL: Trop d'arguments"))
	      (case (car args)
		('&optional (optional (cdr args) params))
		('&rest (rest (cdr args) params))
		('&key (append (keyinit (cdr args)) (key (cdr args) params)))
		(t (if (null params)
		       (error "METAEVAL: Pas assez d'arguments")
		     (cons (list (car args) (meval (car params) ensvar ensfct)) (fctargs (cdr args) (cdr params) ensvar ensfct))))))))

; * teste si un mot-clef est valide

(mdefun validkey (key lstkeys)
	(if (null lstkeys)
	    nil
	  (if (consp (car lstkeys))
	      (if (equal (string key) (string (caar lstkeys)))
		  (caar lstkeys)
		(validkey key (cdr lstkeys)))
	    (if (equal (string key) (string (car lstkeys)))
		(car lstkeys)
	      (validkey key (cdr lstkeys))))))

; * renverse une liste
;(mdefun revlst (lst)
;  (if (null (cdr lst)) lst
;    (append (revlst (cdr lst)) (list (car lst)))))

()