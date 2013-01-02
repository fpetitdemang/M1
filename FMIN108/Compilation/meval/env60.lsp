; ***** initialisation des environnements *****
;- l'environnement est implémenté par une liste de 2 listes d'associations
;- la dernière liste d'associations correspond aux variables ou fonctions globales
;- la première liste d'associations correspond aux variables et fonctions locales qui sont prioritaires (masquent) lors de la recherche d'une valeur sur les variables et fonctions globales 
;- les assaciations locales successives sont rajoutées en tete de la première liste d'association
;- j'ai choisi cette implémentation pour faciliter la recherche de valeurs: on cherche une valeur dans l'association locale, la plus récente sera la première trouvée et enfin si on n'a pas trouvé la valeur, on cherche dans la liste d'associations globales 
;- j'utilise 2 variables d'environnement: 1 pour les variables et constantes, et 1 pour les macros et fonctions
;- pourquoi 2 variables?
;- mon choix est basé sur le fait que l'on peut avoir une variable et une fonction qui portent le même nom (si le symbole est en tête de liste il est considére comme une fonction et sinon il est condsidéré comme une variable. il fallait donc que les variables et fonctions ne partagent pas le même environnement)
;- pourquoi stoquer ensemble variables et constantes, fonctions et macros?
;- du moment qu'un symbole n'est pas en tête de liste, il est considéré comme une variable OU une constante mais pas les 2! donc, il est inutile (et surtout gênant!...) de stoquer 2 fois un symbole qui ne pourra être qu'une chose à la fois! (surtout que une variable pouvant être redéfinie comme une constante, il faudrait alors effacer la variable pour éviter toute confusion...)
;- pour les fonctions et macros, c'est exactement la même chose! un symbole en tête de liste sera une fonction OU une macro mais pas les 2! une fonction peut être redéfinie en macro et vice-versa


(defparameter debug nil)
(defvar ensvar '(()()))
(setf ensvar '(()()))
(defvar ensfct '(()()))
(setf ensfct '(()()))

; ***** fonctions de base définies dans l'environnement CLISP, nécessaires à la définition de la fonction MDEFUN *****

;sert à définir une fonction dans les environnements METAEVAL et CLISP

(defmacro mdefun (nom args &rest corps)
  `(prog1
     (defun ,nom ,args ,@corps)
     (setglobalfunction (quote ,nom) (quote (,args ,corps)) (quote ,ensvar) (quote ,ensfct))))

;définit une fonction globale dans l'environnement METAEVAL

(defun setglobalfunction (expr val ensvar ensfct)
  (let ((r (assoc expr (cadr ensfct))))
    (if r
	(cons expr (setf (cdr r) (list val (car ensvar) 0)))
      (car (setf (car (cdr ensfct)) (cons (list expr val (car ensvar) 0) (cadr ensfct)))))))

(mdefun setglobalfunction (expr val ensvar ensfct)
	(let ((r (assoc expr (cadr  ensfct))))
	  (if r
	      (cons expr (setf (cdr r) (list val (car ensvar) 0)))
	    (car (setf (car (cdr ensfct)) (cons (list expr val (car ensvar) 0) (cadr ensfct)))))))

; * renvoie la liste d'associations avec les variables locales
(mdefun varassoc (vars ensvar ensfct)
	(if (null vars)
	     nil
	   (if (atom (car vars))
	       (cons (list (car vars) nil) (varassoc (cdr vars) ensvar ensfct))
	     (cons (list (caar vars) (meval (cadar vars) ensvar ensfct )) (varassoc (cdr vars) ensvar ensfct)))))

; * modifie la 1ère variable locale trouvée en parcourant les associations locales dans l'ordre chronologique inverse de leur création, ou à defaut, la modifie ou la crée dans les associations globales

(mdefun setlocalvaleur (expr val ensvar ensfct )
	(if (or (cstvaleur expr ensvar ensfct) (constantp expr))
	    (error "METAEVAL: impossible de modifier une constante")
	  (let ((r (assoc expr (car ensvar))))
	    (if r
		(progn
		  (setf (cdr r) (list val))
		  (assoc expr (car ensvar)))
	      (let ((r (assoc expr (cadr ensvar))))
		(if r
		    (setf (cdr r) (list val))
		  (setf (car (cdr ensvar)) (cons (list expr val) (car ensvar))))
		r)))))

; * renvoie les paramètres et le corps d'une fonction ou macro dans l'environnement METAEVAL ou nil si elle n'est pas définie

(mdefun fctvaleur (expr ensvar ensfct )
	(let ((r (assoc expr (car ensfct))))
	  (if r
	      r
	    (assoc expr (cadr ensfct)))))

; * renvoie la liste des arguments

(mdefun fargs (fv) (caadr fv))

; * renvoie le corps

(mdefun fcorps (fv) (cadadr fv))

; * renvoie l'environnement de variables

(mdefun fensvar (fv) (caddr fv))

; * crée ou modifie une variable globale si ce n'est pas une constante

(mdefun setglobalvaleur (expr val ensvar ensfct )
	(if (or (cstvaleur expr ensvar ensfct) (constantp expr))
	    (error "METAEVAL: impossible de modifier une constante")
	  (let ((r (assoc expr (cadr ensvar))))
	    (if r
		(list expr (setf (car (cdr r)) val))
	      (car (setf (car (cdr ensvar)) (cons (list expr val) (cadr ensvar))))))))

; * modifie une constante

(mdefun setconstant (expr val enxvar ensfct)
	(let ((r (cstvaleur expr ensvar ensfct)))
	  (if r
	      (list expr (setf (car (cdr r)) val))
	    (car (setf (car (cdr ensvar)) (cons (list expr val 'const) (cadr ensvar)))))))
	

; * renvoie les paramètres et le corps d'une macro dans l'environnement METAEVAL ou nil si elle n'est pas définie

(mdefun macrovaleur (expr ensvar ensfct)
	(let ((r (fctvaleur expr ensvar ensfct)))
	  (if r
	      (if (equal (mod (fdrap r) 2) 1)
		  r))))

; * renvoie la valeur d'une variable ou constante dans l'environnement METAEVAL ou nil si elle n'est pas définie

(mdefun varvaleur (expr ensvar ensfct )
	(let ((r (assoc expr (car ensvar))))
	  (if r
	      r
	    (assoc expr (cadr ensvar)))))

; * renvoie la valeur d'une constante dans l'environnement METAEVAL ou nil si elle n'est pas définie

(mdefun cstvaleur (expr ensvar ensfct)
	(let ((r (varvaleur expr ensvar ensfct)))
	  (if (eq (caddr r) 'const)
	      r)))

; * sert à définir une macro dans les environnements METAEVAL et CLISP

(defmacro mdefmacro (nom args &rest corps)
  `(prog1
       (defmacro ,nom ,args ,@corps)
     (setmacro (quote ,nom) (quote (,args ,corps)) (quote ,ensvar) (quote ,ensfct))))

; * définit une macro dans l'environnement METAEVAL

(mdefun setmacro (expr val ensvar ensfct)
	(setf (car (cdddr (setglobalfunction expr val ensvar ensfct))) 1))

; pour encapsuler

(mdefun fdrap (fv) (cadddr fv))

(mdefun fcompiledp (fv) (if fv (>= (fdrap fv) 2)))

(mdefun setcompf (nom args corps &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if r (setf (cdr r) (list (list args corps) (car ensvar) 2))
	    (setf (car (cdr ensfct)) (cons (list nom (list args corps) (car ensvar) 2) (cadr ensfct))))))

(mdefun setcompm (nom args corps &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if r (setf (cdr r) (list (list args corps) (car ensvar) 3))
	    (setf (car (cdr ensfct)) (cons (list nom (list args corps) (car ensvar) 3) (cadr ensfct))))))

(mdefun is-compiled-macro (nom &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (macrovaleur nom ensvar ensfct)))
	  (if (and r (fcompiledp r))
	      r)))

(mdefun is-compiled-function (nom &optional (ensvar ensvar) (ensfct ensfct))
	(let ((r (fctvaleur nom ensvar ensfct)))
	  (if (and r (fcompiledp r))
	      r)))

(mdefun makefv (nom args corps &optional (ensvar ensvar) (ensfct ensfct))
	(list nom (list args corps) ensvar 0))

(mdefun macrop (fv)
	(if fv (= (mod (fdrap fv) 2) 1)))

(mdefun makevv (nom val) (list nom val))

(mdefun local (ens) (car ens))

(mdefun global (ens) (cadr ens))

