;declaration des registres
;+initiailisation


;implementation de la vm
;construit les elements de la vm
(defun make-vm(nom-vm taille)
  (progn 
    (setf (get nom-vm ':RO) 1)
    (setf (get nom-vm ':MEM) (make-array (list taille):initial-element (list 'NOP))) 
    (setf (get nom-vm ':BP) 0)
    (setf (get nom-vm ':SP) 0)
    (setf (get nom-vm ':RO) 0)
    (setf (get nom-vm ':R1) 0)
    )
  )

(defun exec-vm(instruction)
  ;analyse par cas des instructions
  ;(case (car intruction)
  ;  (move ...)
  ;  (push ...)
  )

;charge le code dans le registre d'instruction
;nom-vm : la vm que l'on manipule
;code : liste plate des intructions 
(defun load-vm(nom-vm code)
  (loop for instruction in code
	do
	;ecrit le code dans la memoire
	(ecrire-mem nom-vm (get-registre nom-vm ':SP) instruction)
	;incremte le SP
	(set-registre nom-vm ':SP (+ (get-registre nom-vm ':SP) 1))
	)
  )

;recupere la valeur d'un registre
(defun get-registre(nom-vm registre) 
  (get nom-vm registre))

;ecrit dans un registre
(defun set-registre(nom-vm registre valeur)
  (setf (get nom-vm registre) valeur))

;recupere une instance de memoire
(defun get-mem(nom-vm) 
  (get nom-vm ':MEM))

;lit dans la memoire de la vm
(defun lire-mem(nom-vm adresse)
  (aref nom-vm  adresse))

;ecrit dans le memoire de la vm
(defun ecrire-mem(nom-vm adresse valeur)
  (setf (aref (get-mem nom-vm) adresse) valeur))

;affiche la memoire de la machine virtuel
(defun affiche-mem(nom-vm)

  (loop for i from 0 to (- (array-total-size (get nom-vm ':MEM)) 1)
	do
	(format t "cellule : ~s ~%" (lire-mem (get nom-vm ':MEM) i))
  )
)
