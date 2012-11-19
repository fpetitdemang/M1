(defun case-analysis-bis (expr env)

  (if (atom expr)
      (if (constantp expr)
	  (format t "litteral ~s ~%" expr)
	
	(if (find expr env)
	    (format t "variable d'environnement ~s ~%" expr)))

    (if (consp (car expr))
	(if (eq ’lambda (car expr))
	    (format t "fonction lambda ~s ~%" expr)
	  (error "une expression évaluable ne commence pas par (( : ~s" expr))
      
      (if (not (symbolp (car expr)))
	  (error "~s n’est pas un symbole" (car expr))
	;; (CAR EXPR) est un symbole
	;; ici s’insère le cas des fonctions et macros méta-définies
	(if (not (fboundp (car expr)))
	    (error "~s fonction inconnue" (car expr))
	  ;; (CAR EXPR) est un symbole qui joue le rôle de fonction
	  (if (macro-function (car expr))
	      (case-analysis-bis (macroexpand-1 expr))
					; macros
	    (if (special-form-p (car expr))
		(case (car expr)
		      (if
			  (format t "fonction  ~s ~%" expr))
					; formes syntaxiques
					; ⇒ récursion sur certains arguments
		      (t
					; pour rattraper ce qui n’est pas encore implémenté
		       (error "forme spéciale NYI ~s" (car expr))))
					; fonctions normales
					; ⇒ récursion sur tous les arguments
					; ⇒ éventuellement construction d’environnement
					; ⇒ et analyse du corps de la fonction appelée
	      )))))))