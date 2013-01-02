; FONCTIONS D'INTERFACAGE AVEC LE COMPILATEUR ET LA MACHINE VIRTUELLE


; name : nom d'une fonction ou macro compilee

; resultat : charge le code de la fonction ou macro 'name' dans la MACHINE VIRTUELLE;
; renvoie vrai si le chargement a reussi, faux sinon

(mdefun vm-load-function(name)
  (let ((func (get-compiled-function name :include-macros t)))
    (if func
	; 'charge-code' est une fonction de la MACHINE VIRTUELLE
	(progn (out "*** fonction ou macro ~W chargee dans la MACHINE VIRTUELLE ***~%" name)
	       (charge-code (extract-body func) name)
	       t)
      (progn (warn "aucune fonction ou macro compilee de nom ~W~%" name)
	     nil)))
  )


; name : nom d'une fonction ou macro compilee
; lparams : la liste des parametres a passer a la fonction ou macro
; debug : indique si la machine virtuelle doit afficher ou non les instructions executees

; resultat : fait executer a la machine virtuelle le code de la fonction ou macro 'name'
; avec les parametres 'lparam'; la fonction suppose que le code de la fonction ou macro a ete
; charge dans la MACHINE VIRTUELLE; renvoie le resultat de l'evaluation par la MACHINE VIRTUELLE

(mdefun vm-apply(name lparams &optional (debug t))
  (cond
   ((not (symbolp name))
    		(error "vm-apply: ~W n'est pas un nom de fonction" name))
   
   ((not (listp lparams))
    		(error "vm-apply: ~W n'est une liste de parametres" lparams))

   ; 'lance-code-appel' est une fonction de la MACHINE VIRTUELLE qui prend en argument
   ; le code qui sert a demarrer l'execution
   (t		(lance-code-appel (vm-compile `(,name ,@lparams)) debug)))
  )


; name : nom d'une fonction mdefinie

; resultat : compile la fonction 'name' et la charge dans la MACHINE VIRTUELLE

(mdefun vm-compile-function(name)
  (let ((func (get-mdefined-function name :macros-only t)))
    (cond
     (func (vm-compile `(defmacro ,name ,(extract-prototype func) ,@(extract-body func)))
	   (vm-load-function name))
     
     (t (setf func (get-mdefined-function name))
	(cond
	 (func (vm-compile `(defun ,name ,(extract-prototype func) ,@(extract-body func)))
	       (vm-load-function name))
	 
	 (t (warn "aucune fonction ou macro mdefinie de nom ~W~%" name)
	    nil)))))
  )

