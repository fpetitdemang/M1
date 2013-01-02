; FONCTIONS D'ENREGISTREMENT ET DE RECUPERATION DES FONCTIONS COMPILEES
;
; rem :
;	- toutes ces fonctions ne sont que des interfaces pour des fonctions du META-EVALUATEUR ;
; 	- une definition de fonction/macro est une structure de donnees privee du META-EVALUATEUR ;
; contenant le nom de la fonction/macro, son prototype (voir fichier 'gen_proto.lisp'), son code,
; ainsi qu'un moyen de differencier les fonctions des macros. On peut recuperer les informations via des
; du META-EVALUATEUR ou des fonctions d'interface ci-dessous
;	- une fonction ne peut etre a la fois mdefinie et compilee


; name : nom d'une fonction
; prototype : prototype d'une fonction
; body : une liste d'instructions en pseudo-assembleur
; is-macro : indique si on enregistre une macro ou une fonction

; resultat : enregistre une fonction avec son prototype
; et son corps dans l'environnement du META-EVALUATEUR

(mdefun add-compiled-function(name prototype body &key (is-macro nil))
  (if is-macro
      (setcompm name prototype body ensvar ensfct)
    (setcompf name prototype body ensvar ensfct))
  )


; name : nom d'une fonction
; include-macros : obsolete (sert uniquement pour la compatibilite)
; macros-only : indique si la fonction doit uniquement s'interesser aux macros, ou si
; elle considere aussi bien les macros que les fonctions

; resultat : s'il existe une fonction/macro compilee de nom 'name' enregistree dans
; l'environnement du META-EVALUATEUR, renvoie sa definition, et nil s'il
; n'y en a pas

(mdefun get-compiled-function(name &key (include-macros nil) (macros-only nil))
    (cond (macros-only (is-compiled-macro name))
     
	  (t (is-compiled-function name)))
  )


; name : nom d'une fonction
; include-macros : obsolete (sert uniquement pour la compatibilite)
; macros-only : indique si la fonction doit uniquement s'interesser aux macros, ou si
; elle considere aussi bien les macros que les fonctions

; resultat : s'il existe une fonction/macro compilee de nom 'name' enregistree dans
; l'environnement du META-EVALUATEUR, renvoie son code, et nil s'il
; n'y en a pas

(mdefun get-compiled-function-body(name &key (include-macros nil) (macros-only nil))
  (let ((funcDef (get-compiled-function name :include-macros include-macros :macros-only macros-only)))
    (extract-body funcDef))
  )


; name : nom d'une fonction
; include-macros : obsolete (sert uniquement pour la compatibilite)
; macros-only : indique si la fonction doit uniquement s'interesser aux macros, ou si
; elle considere aussi bien les macros que les fonctions

; resultat : s'il existe une fonction/macro compilee de nom 'name' enregistree dans
; l'environnement du META-EVALUATEUR, renvoie son prototype, et nil s'il
; n'y en a pas

(mdefun get-compiled-function-prototype(name &key (include-macros nil) (macros-only nil))
  (let ((funcDef (get-compiled-function name :include-macros include-macros :macros-only macros-only)))
    (extract-prototype funcDef))
  )


; name : nom d'une fonction
; macros-only : indique si la fonction doit uniquement s'interesser aux macros, ou si
; elle considere aussi bien les macros que les fonctions

; resultat : s'il existe une fonction/macro mdefinie de nom 'name' enregistree dans
; l'environnement du META-EVALUATEUR, renvoie sa definition, et nil s'il n'y en a pas

(mdefun get-mdefined-function(name &key (macros-only nil))
  ; 'macrovaleur' est une fonction du META-EVALUATEUR qui renvoie une definition de macro
  ; 'ensvar' et 'ensfct' sont deux variables utilisees par le META-EVALUATEUR
  ; pour stocker l'environnement
  (let ((funcDef (macrovaleur name ensvar ensfct)))
    (cond
     ; 'fcompiledp' est une fonction du META-EVALUATEUR
     ((and (consp funcDef)
	   (not (fcompiledp funcDef)))
      funcDef)
	   
     (macros-only nil)
	   
     ; 'fctvaleur' est une fonction du META-EVALUATEUR qui renvoie une definition de fonction
     (t (setf funcDef (fctvaleur name ensvar ensfct))
	(if (and (consp funcDef)
		 (not (fcompiledp funcDef)))
	    funcDef
	  nil))))
  )


; funcDef : definition d'une fonction

; resultat : renvoie le corps de la fonction, i.e. son code en LISP si la fonction n'est pas compilee,
; ou son code en pseudo-assembleur si elle l'est

(mdefun extract-body(funcDef)
	(if (atom funcDef)
	    (warn "extract-body : definition de fonction incorrecte (recu : ~W)" funcDef)
	  ; 'fcorps' est une fonction du META-EVALUATEUR
	  (fcorps funcDef))
	)


; funcDef : definition d'une fonction

; resultat : renvoie la liste des arguments de la fonction si elle n'est pas compilee,
; ou son prototype si elle l'est

(mdefun extract-prototype(funcDef)
	(if (atom funcDef)
	    (warn "extract-prototype : definition de fonction incorrecte (recu : ~W)" funcDef)
	  ; 'fargs' est une fonction du META-EVALUATEUR
	  (fargs funcDef))
	)
