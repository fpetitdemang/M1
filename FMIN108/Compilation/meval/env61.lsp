; ***** initialisation des environnements *****
;- l'environnement est impl�ment� par une liste de 2 listes d'associations
;- la derni�re liste d'associations correspond aux variables ou fonctions globales
;- la premi�re liste d'associations correspond aux variables et fonctions locales qui sont prioritaires (masquent) lors de la recherche d'une valeur sur les variables et fonctions globales 
;- les assaciations locales successives sont rajout�es en tete de la premi�re liste d'association
;- j'ai choisi cette impl�mentation pour faciliter la recherche de valeurs: on cherche une valeur dans l'association locale, la plus r�cente sera la premi�re trouv�e et enfin si on n'a pas trouv� la valeur, on cherche dans la liste d'associations globales 
;- j'utilise 2 variables d'environnement: 1 pour les variables et constantes, et 1 pour les macros et fonctions
;- pourquoi 2 variables?
;- mon choix est bas� sur le fait que l'on peut avoir une variable et une fonction qui portent le m�me nom (si le symbole est en t�te de liste il est consid�re comme une fonction et sinon il est condsid�r� comme une variable. il fallait donc que les variables et fonctions ne partagent pas le m�me environnement)
;- pourquoi stoquer ensemble variables et constantes, fonctions et macros?
;- du moment qu'un symbole n'est pas en t�te de liste, il est consid�r� comme une variable OU une constante mais pas les 2! donc, il est inutile (et surtout g�nant!...) de stoquer 2 fois un symbole qui ne pourra �tre qu'une chose � la fois! (surtout que une variable pouvant �tre red�finie comme une constante, il faudrait alors effacer la variable pour �viter toute confusion...)
;- pour les fonctions et macros, c'est exactement la m�me chose! un symbole en t�te de liste sera une fonction OU une macro mais pas les 2! une fonction peut �tre red�finie en macro et vice-versa


(defparameter ensvar '(()()))
(defparameter ensfct '(()()))

; ***** fonctions de base d�finies dans l'environnement CLISP, n�cessaires � la d�finition de la fonction MDEFUN *****

; * sert � d�finir une fonction dans les environnements METAEVAL et CLISP

(defmacro mdefun (nom params &rest corps)
  `(prog1
     (defun ,nom ,params ,@corps)
     (setglobalfunction (quote ,nom) (quote ,params) (quote ,corps) (quote ,ensvar) (quote ,ensfct))))

; * d�finit une fonction globale dans l'environnement METAEVAL

(defun setglobalfunction (nom params corps ensvar ensfct)
  (let ((r (getv nom (global ensfct))))
    (if r
	(cons nom (setf (cdr r) (cdr (makefv nom params corps ensvar ensfct))))
      (car (setf (car (cdr ensfct)) (addproto (makefv nom params corps ensvar ensfct) (global ensfct)))))))

; * r�cup�re le prototype d'une variable

(defun getv (nom env) (assoc nom env))
	
; * ajoute un �l�ment � un environnement local ou global

(defun addproto (elt env) (cons elt env))

; * renvoie l'environnement global

(defun global (ens) (cadr ens))

; * renvoie l'environnement local

(defun local (ens) (car ens))

; * cr�e un prototype de fonction

(defun makefv (nom params corps &optional (ensvar ensvar) (ensfct ensfct))
  (list nom (list params corps) (list (local ensvar)) 0))

; * m�ta-d�finition des d�finitions initiales maintenant que l'on a mdefun

(mdefun getv (nom env) (assoc nom env))

(mdefun makefv (nom params corps &optional (ensvar ensvar) (ensfct ensfct))
  (list nom (list params corps) (list (local ensvar) (local ensfct)) 0))

(mdefun global (ens) (cadr ens))

(mdefun addproto (elt env) (cons elt env))

(mdefun setglobalfunction (nom params corps ensvar ensfct)
  (let ((r (getv nom (global ensfct))))
    (if r
	(cons nom (setf (cdr r) (cdr (makefv nom params corps ensvar ensfct))))
      (car (setf (car (cdr ensfct)) (addproto (makefv nom params corps ensvar ensfct) (global ensfct)))))))

(mdefun local (ens) (car ens))

; * renvoie la liste d'associations avec les variables locales

(mdefun varassoc (vars ensvar ensfct)
	(if (null vars)
	     nil
	   (if (atom (car vars))
	       (cons (list (car vars) nil) (varassoc (cdr vars) ensvar ensfct))
	     (cons (list (caar vars) (meval (cadar vars) ensvar ensfct )) (varassoc (cdr vars) ensvar ensfct)))))

; * modifie la 1�re variable locale trouv�e en parcourant les associations locales dans l'ordre chronologique inverse de leur cr�ation, ou � defaut, la modifie ou la cr�e dans les associations globales

(mdefun setlocalvaleur (expr val ensvar ensfct )
	(if (or (cstvaleur expr ensvar ensfct) (constantp expr))
	    (error "METAEVAL: impossible de modifier une constante")
	  (let ((r (getv expr (local ensvar))))
	    (if r
		(progn
		  (setf (cdr r) (list val))
		  (getv expr (local ensvar)))
	      (let ((r (getv expr (global ensvar))))
		(if r
		    (setf (cdr r) (list val))
		  (setf (car (cdr ensvar)) (cons (list expr val) (local ensvar))))
		r)))))

; * renvoie les param�tres et le corps d'une fonction ou macro dans l'environnement METAEVAL ou nil si elle n'est pas d�finie

(mdefun fctvaleur (expr ensvar ensfct )
	(let ((r (getv expr (local ensfct))))
	  (if r
	      r
	    (getv expr (global ensfct)))))

; * renvoie la liste des param�tres

(mdefun fargs (fv) (caadr fv))

; * renvoie le corps

(mdefun fcorps (fv) (cadadr fv))

; * renvoie l'environnement de variables

(mdefun fensvar (fv) (caaddr fv))

; * renvoie l'environnement de fonctions

(mdefun fensfct (fv) (car (cdaddr fv)))

; * cr�e ou modifie une variable globale si ce n'est pas une constante

(mdefun setglobalvaleur (expr val ensvar ensfct )
	(if (or (cstvaleur expr ensvar ensfct) (constantp expr))
	    (error "METAEVAL: impossible de modifier une constante")
	  (let ((r (getv expr (global ensvar))))
	    (if r
		(list expr (setf (car (cdr r)) val))
	      (car (setf (car (cdr ensvar)) (cons (list expr val) (global ensvar))))))))

; * modifie une constante

(mdefun setconstant (expr val enxvar ensfct)
	(let ((r (cstvaleur expr ensvar ensfct)))
	  (if r
	      (list expr (setf (car (cdr r)) val))
	    (car (setf (car (cdr ensvar)) (cons (list expr val 'const) (global ensvar)))))))
	

; * renvoie les param�tres et le corps d'une macro dans l'environnement METAEVAL ou nil si elle n'est pas d�finie

(mdefun macrovaleur (expr ensvar ensfct)
	(let ((r (fctvaleur expr ensvar ensfct)))
	  (if r
	      (if (equal (mod (fdrap r) 2) 1)
		  r))))

; * renvoie la valeur d'une variable ou constante dans l'environnement METAEVAL ou nil si elle n'est pas d�finie

(mdefun varvaleur (expr ensvar ensfct )
	(let ((r (getv expr (local ensvar))))
	  (if r
	      r
	    (getv expr (global ensvar)))))

; * renvoie la valeur d'une constante dans l'environnement METAEVAL ou nil si elle n'est pas d�finie

(mdefun cstvaleur (expr ensvar ensfct)
	(let ((r (varvaleur expr ensvar ensfct)))
	  (if (eq (caddr r) 'const)
	      r)))

; * sert � d�finir une macro dans les environnements METAEVAL et CLISP

(defmacro mdefmacro (nom params &rest corps)
  `(prog1
       (defmacro ,nom ,params ,@corps)
     (setmacro (quote ,nom) (quote ,params) (quote ,corps) (quote ,ensvar) (quote ,ensfct))))

; * d�finit une macro dans l'environnement METAEVAL

(mdefun setmacro (nom params corps ensvar ensfct)
	(setf (car (cdddr (setglobalfunction nom params corps ensvar ensfct))) 1))

; accesseurs

; * renvoie le drapeau caract�risant la fonction

(mdefun fdrap (fv) (cadddr fv))

; * teste si la fonction est compil�e

(mdefun fcompiledp (fv) (if fv (>= (fdrap fv) 2)))

; * marque la fonction comme compil�e

(mdefun setcompf (nom params corps &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if r (setf (cdr r) (list (list params corps) (local ensvar) 2))
	    (setf (car (cdr ensfct)) (cons (list nom (list params corps) (local ensvar) 2) (global ensfct))))))

; * marque la macro comme compil�e

(mdefun setcompm (nom params corps &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if r (setf (cdr r) (list (list params corps) (local ensvar) 3))
	    (setf (car (cdr ensfct)) (cons (list nom (list params corps) (local ensvar) 3) (global ensfct))))))

; * teste si la macro est compil�e

(mdefun is-compiled-macro (nom &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (macrovaleur nom ensvar ensfct)))
	  (if (and r (fcompiledp r))
	      r)))

; * teste si la fonction est compil�e

(mdefun is-compiled-function (nom &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if (and r (fcompiledp r))
	      r)))

; teste si c'est une macro

(mdefun macrop (fv)
	(if fv (= (mod (fdrap fv) 2) 1)))

; cr�e un prototype de variable

(mdefun makevv (nom val) (list nom val))

; cr�e un environnement

(mdefun makeenv (local global) (list local global))

; unifie 2 environnements

(mdefun unionenv (env1 env2) (append env1 env2))

; renvoie la valeur d'un prototypage de valeur

(mdefun vval (vv) (cadr vv))