

(defvar registres (make-hash-table))
(defvar liste-registres)

(defvar variables-globales (make-hash-table)) ;rien de prévu pour une introduction encapsulée des variables


(setf liste-registres '(A0 A1 A2 A3 A4 A5 SP FP PC))

(mdefun reset () ;trivial
  (progn
    (setf (gethash 'AA registres) 0) ;relicquat d'une séance mémorable de débogage le 26 novembre 1999 avec Gauthier ! 
    (setf (gethash 'A0 registres) 0)
    (setf (gethash 'A1 registres) 0)
    (setf (gethash 'A2 registres) 0)
    (setf (gethash 'A3 registres) 0)
    (setf (gethash 'A4 registres) 0)
    (setf (gethash 'A5 registres) 0)
    (setf (gethash 'SP registres) -1)
    (setf (gethash 'FP registres) -1)
    (setf (gethash 'PC registres) -1)
    )
)


(mdefun affecte-registre (reg valeur)
  (if (member reg liste-registres)
      (setf (gethash reg registres) valeur)
    (format *standard-output* "~S n'est pas un registre~%" reg)))

(mdefun retourne-valeur (machin)
  
  (if (symbolp  machin) ; si c'est un symbole
      
      (if (member machin liste-registres) ;soit un registre
	  (gethash machin registres)
	;Si c'est pas un registre c'est une variabble
	(gethash machin variables-globales)
	)
    
  (if (not (functionp machin))
      machin
    (format *standard-output* "~S est une fonction clisp~%" machin)   
    )
  )
)

; gestion des registres et de la pile

(defconstant stack-size 16384)

; SP modifie avant operation ( SP = -1: pile vide; SP = stack-size: pile pleine )
; PSW: modifier par une fonction de comparaison


(defvar stack (make-array stack-size))

(mdefun gpush(x)
  (if (< (retourne-valeur 'SP) (1- stack-size))
      (progn (setf (gethash 'SP registres) (1+ (retourne-valeur 'SP))) 
	     (setf (elt stack (retourne-valeur 'SP)) x))
    (error "push: pile pleine")))
  

(mdefun gpop()
  (if (> (retourne-valeur 'SP) -1)
      (progn (setf (gethash 'SP registres) (1- (retourne-valeur 'SP)))
	     (elt stack (1+ (retourne-valeur 'SP))))
    (error "pop: pile vide"))
  )

(mdefun retourne-pile(indice)
  (elt stack (+ (gethash 'fp registres) indice)))

(mdefun affecte-pile ( indice valeur)
  (setf (elt stack (+ (gethash 'fp registres) indice)) valeur))

;encapsulation pour l'avenir
(mdefun affecte-direct-pile (indice valeur)
	(setf (elt stack indice) valeur))

(mdefun retourne-direct-pile (indice)
	(elt stack indice))