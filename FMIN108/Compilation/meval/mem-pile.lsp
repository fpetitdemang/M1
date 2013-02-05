

(defvar registres (make-hash-table))
(defvar liste-registres)

(defvar variables-globales (make-hash-table)) ;rien de prévu pour une introduction encapsulée des variables
(defvar liste-variables-globales nil)

(setf liste-registres '(A0 A1 A2 A3 A4 A5 SP FP PC))

(mdefun reset () ;trivial
  (progn
    (setf (gethash 'AA registres) 0) ;relicquat d'une séance mémorable de débogage le 26 novembre 1999 avec Gauthier ! 
    (setf (gethash 'A0 registres) 0) ;C'est bien a"Zéro" et pas a"eau"!
    (setf (gethash 'A1 registres) 0)
    (setf (gethash 'A2 registres) 0)
    (setf (gethash 'A3 registres) 0)
    (setf (gethash 'A4 registres) 0)
    (setf (gethash 'A5 registres) 0)
    (setf (gethash 'SP registres) TailleMemoire)
    (setf (gethash 'FP registres) TailleMemoire)
    (setf (gethash 'PC registres) -1)
    )
)
(reset)

(mdefun affecte-registre (reg valeur)
  (if (member reg liste-registres)
      (setf (gethash reg registres) valeur)
    (format *standard-output* "~S n'est pas un registre~%" reg)))

(mdefun retourne-valeur (machin) ;Retourne la valeur de tout ce que l'on veut !
  
  (if (symbolp  machin) ; si c'est un symbole
      
      (if (member machin liste-registres) ;soit un registre
	  (gethash machin registres)
	;Si c'est pas un registre c'est une variable
	(gethash machin variables-globales)
	)
    
  (if (not (functionp machin))
      machin
    (format *standard-output* "~S est une fonction clisp~%" machin)   
    )
  )
)
()













