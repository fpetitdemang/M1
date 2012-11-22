


;construit les elements de la vm
(defun make-vm(nom-vm taille)
  (progn 
    (setf (get nom-vm ':TAILLE) taille)
    (setf (get nom-vm ':MEM) (make-array (list taille))) 
    (setf (get nom-vm ':BP) 0)
    (setf (get nom-vm ':SP) 0)
    (setf (get nom-vm ':MAXSP) 0)
    (setf (get nom-vm ':PC) (- taille 1))
    (setf (get nom-vm ':RO) 0)
    (setf (get nom-vm ':R1) 0)
    )
  )

;lance la vm a partir de l'adresse main
(defun exec-vm(nom-vm main)
  ;initialise le PC 
  ;(set-registre nom-vm ':PC main)
  
  ;recupere la fonction + src/dest de l'instruction
  (let( (fun (car (lire-mem nom-vm (get-registre nom-vm ':PC)))) (args (cdr (lire-mem nom-vm (get-registre nom-vm ':PC))))))

  ;analyse par cas des instructions
  (loop for i from 0 to (- (get-registre 'nom-vm ':TAILLE) 1)
	do
      (case (car (lire-mem nom-vm (get-registre nom-vm ':PC)))
	(t
	 (format t "instruction : ~s ~%"  (car (lire-mem nom-vm (get-registre nom-vm ':PC))))
	 (format t "src : ~s ~%"  (car (cdr (lire-mem nom-vm (get-registre nom-vm ':PC)))))
	 (format t "dest : ~s ~%" (cdr (cdr (lire-mem nom-vm (get-registre nom-vm ':PC)))))
	 )
	)
      ;decrement PC
      (DECR ':PC)
      
      )
  
  )

;charge le code dans le registre d'instruction. !ecrase le code deja charger!
;nom-vm : la vm que l'on manipule
;code : liste plate des intructions 
(defun load-vm(nom-vm code)
  (loop for instruction in code
	do
	;ecrit le code dans la memoire
	(ecrire-mem nom-vm (get-registre nom-vm ':PC) instruction)
	;decremte le PC
	(set-registre nom-vm ':PC (- (get-registre nom-vm ':PC) 1))
	)

  ;reset PC sur la premiere instruction a executer
  (set-registre nom-vm ':PC (- (get-registre nom-vm ':TAILLE) 1))
  )




;FONCTION MANIPULATION DE LA VM 

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
  (aref (get-mem nom-vm)  adresse))

;ecrit dans le memoire de la vm
(defun ecrire-mem(nom-vm adresse valeur)
  (setf (aref (get-mem nom-vm) adresse) valeur))

;affiche la memoire de la machine virtuel
(defun affiche-mem(nom-vm)

  (loop for i from 0 to (- (array-total-size (get nom-vm ':MEM)) 1)
	do
	(format t "cellule : ~s ~%" (lire-mem nom-vm i)))) 




;OPERATION DE LA VM A REGISTRE

(defun my-LOAD (nom-vm src dest);memoire->registre
  (set-registre nom-vm dest src))

(defun my-STORE (nom-vm src dest);registre->memoire
  (ecrire-mem nom-vm dest (get-registre nom-vm src)))

(defun MOVE (nom-vm src dest);registre->registre
  (my-LOAD nom-vm (get-registre nom-vm src) dest))

(defun INCR (nom-vm src);incremete registre
  (set-registre nom-vm src (+ 1 (get-registre nom-vm src))))
 
(defun DECR (nom-vm src);decrement registre
  (set-registre nom-vm src (- (get-registre nom-vm src) 1))) 

(defun ADD (nom-vm src dest);ajoute src+dest dans dest
  (set-registre nom-vm dest (+ (get-registre nom-vm src) (get-registre nom-vm dest))))













