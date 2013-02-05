#|

  TER 141 2005-2006
  Machine virtuelle
  mv.lsp

  Lebec Laurie : laurie.lebec@gmail.com
  Leture Julien : julien@leture.com

|#


; (load "~/Documents/Cours/Lisp/TER 141/mv.lsp")


;*************************************************************************
; MACHINE VIRTUELLE
; la non-utilisation de variables globales permet de lancer plusieurs VM
; le nom passe en parametre permet de differencier les VM
(defun vm-make (&key (nom 'VM1) (taille 10000))
  (let (
        (vm nom) ; nom de la VM
       )
    (setf (get vm 'mem) (make-array taille :initial-element NIL)) ; tableau pour la memoire (on precise que les cases vides sont a NIL)
    (setf (get vm 'etiq) (make-hash-table)) ; hashtable pour les adresses des etiquettes
    (setf (get vm 'ref) (make-hash-table)) ; hashtable pour les references en avant
    (setf (get vm 'R0) 0) ; registre R0
    (setf (get vm 'R1) 0) ; registre R1
    (setf (get vm 'R2) 0) ; registre R2
    (setf (get vm 'SP) (- taille 1)) ; stack pointer > pointe sur la pile (le haut de la memoire)
    (setf (get vm 'BP) (- taille 1)) ; base pointer, ne sera jamais modifie
    (setf (get vm 'FP) 0) ; frame pointer 
    (setf (get vm 'PC) 0) ; program counter > pointe sur le code (le bas de la memoire)
    (format t  "~%>> La machine '~A' a ete creee~%   (taille memoire = ~A)" vm taille)
  )
)

; ok > (vm-make)
; ok > (vm-make :nom 'VM1 :taille 10000)


#|
     *** representation de la memoire ***
     tableau a 1 dimension :
     - le code est au debut (sens croissant)
     - la pile est a la fin (sens decroissant)
     - on ne gere pas le tas (au milieu)

     code -->                     <-- pile
     -------------------------------------
     | | | | | | | | | | | | | | | | | | |
     -------------------------------------
      ^^                               ^^
      PC                               SP
|#


; taille de la memoire
; permet de connaitre la fin de la memoire afin de gerer la pile
(defun vm-memory (vm)
  (- (length (get vm 'mem)) 1)
)

; ok > (vm-memory 'VM1)
; >>>> 9999


; derniere case memoire utilisee
; longueur du code charge - 1
(defun vm-last-address (vm &optional (i 0))
  (if (eq (get-memory vm i) NIL)
      (- i 1) ; car la memoire commence a 0
    (vm-last-address vm (+ i 1))
  )
)

; ok > (vm-last-address 'VM1)


;*************************************************************************
; CHARGEUR
; charge du code dans la vm
; effectue la traduction d'adresses
; traite les references en avant
(defun vm-load (code vm)
  ; si le code a deja ete execute une fois, un HALT a ete ajoute a la fin du code
  ; il faut donc l'enlever (ie. le transformer en NOP)
  (if (recherche vm '(HALT))
      (set-memory vm (recherche vm '(HALT)) '(NOP))
    ()
  )
  (let (
        (i (+ 1 (vm-last-address vm))) ; premiere case libre dans la memoire (ie. derniere utilisee + 1)
       )
    (loop for bout-code in code
          do
          (cond ; si une etiquette (et non un RTN, un NOP ou un HALT !)
                ((and (eq (cdr bout-code) NIL)
                      (not (eq (car bout-code) 'RTN))
                      (not (eq (car bout-code) 'NOP))
                      (not (eq (car bout-code) 'HALT)))
                 (if (get-etiq vm (car bout-code)) ; l'etiquette existe deja > warning !
                     (warn "fonction '~A' deja definie !" (car bout-code))
                   ; sinon on l'ajoute dans la table etiq
                   ; et on traite ses references en avant
                   (progn (set-etiq vm (car bout-code) i)
                          (do-ref vm (car bout-code) i) ; ref avant
                   )
                 )
                )

                ; si une instruction de saut (JMP ou JSR) qui fait reference a une etiquette
                ; ex. (JSR (AT TOTO)) >> (JSR (MEM 18))
                ((or (eq (car bout-code) 'JMP)
                     (eq (car bout-code) 'JSR))
                 (if (eq (caadr bout-code) 'AT) ; on verifie qu'il y a un AT
                     (if (get-etiq vm (cadadr bout-code)) ; si l'etiq existe, on met son adresse a la place
                         (setf bout-code `(,(car bout-code) (MEM,(get-etiq vm (cadadr bout-code))))) ; on modifier le code en mettant l'adresse memoire
                       (set-ref vm (cadadr bout-code) (cons i (get-ref vm (cadadr bout-code)))) ; l'etiq n'existe pas, on l'ajoute dans ref avant
                     )
                   () ; ce n'est pas un AT, rien a traduire
                 )
                )

                ; si une instruction de saut avec une comparaison (JL, JEQ, etc.) qui fait reference a une etiquette
                ; ex. (JLE R0 R1 (AT 2)) >> (JLE R0 R1 (MEM 23))
                ((or (eq (car bout-code) 'JL)
                     (eq (car bout-code) 'JLE)
                     (eq (car bout-code) 'JG)
                     (eq (car bout-code) 'JGE)
                     (eq (car bout-code) 'JEQ)
                     (eq (car bout-code) 'JNE))
                 (if (eq (car (cadddr bout-code)) 'AT) ; on verifie qu'il y a un AT
                     (if (get-etiq vm (cadr (cadddr bout-code))) ; si l'etiq existe, on met son adresse a la place
                         (setf bout-code `(,(car bout-code) (MEM,(get-etiq vm (cadr (cadddr bout-code)))))) ; on modifier le code en mettant l'adresse memoire
                       (set-ref vm (cadr (cadddr bout-code)) (cons i (get-ref vm (cadr (cadddr bout-code))))) ; l'etiq n'existe pas, on l'ajoute dans ref avant
                     )
                   () ; ce n'est pas un AT, rien a traduire
                 )
                )

                ; si une variable globale
                ; (introduite par un setf donc avec un MOVE)
                ((eq (car bout-code) 'MOVE)
                 (let (
                       (dest (caddr bout-code))
                      )
                   (if (atom dest)
                       () ; on ne fait rien
                     (if (eq (car dest) 'EAT)
                         (if (get-etiq vm (cadr dest)) ; la variable existe deja > warning !
                             (warn "variable globale '~A' deja definie !" (cadr dest))
                           ; sinon on l'ajoute dans la table etiq
                           (progn (set-etiq vm (cadr dest) i)
                                  (set-memory vm i 0) ; on met 0 pour le differencier du NIL
                                  (incf i) ; on laisse cette case pour la variable
                           )
                         )
                       () ; sinon on ne fait rien
                     )
                   )
                 )
                )

          )

          ; dans tout les cas, on ajoute le code a la position i
          (progn (set-memory vm i bout-code)
                 (incf i)
          )
    )
    (format t "~%>> Code charge (~A instruction~:P)~%" i) ; astuce : ~:P = pluriel

    ; analyse des ref avant
    (if (eq (hash-table-count (get vm 'ref)) 0)
        (format t ">> Aucun probleme de reference en avant !~%") ; si ref avant vide > ok
      (format t ">> Probleme avec des refecences en avant !~%") ; sinon, si locale "error", si globale "warning"
    )
  )
)

; ok > (vm-load '((MOVE (D 2) R0) (PUSH R0) (MOVE (D 3) R0) (PUSH R0) (POP R1) (POP R0) (ADD R0 R1) (PUSH R0)) 'VM1)
; ok > (vm-load '((MOVE (D 5) R0) (PUSH R0) (MOVE (D 10) R0) (PUSH R0) (POP R1) (POP R0) (SUB R0 R1)) 'VM1)
; >>>> (vm-run-old 'VM1) ; on peut utiliser un lanceur iteratif car il n'y a pas de saut

; ajoute l'instruction HALT a la fin du code charge
; uniquement s'il n'y a pas deja un HALT...
(defun vm-add-halt (vm)
  (if (equal '(HALT) (get-memory vm (vm-last-address vm)))
      ()
    (progn (set-memory vm (+ (vm-last-address vm) 1) '(HALT))
           (format t "~%>> HALT ajoute a la fin du code")
    )
  )
)

; ok > (vm-add-halt 'VM1)


;*************************************************************************
; LANCEMENT DE LA VM
; !! version naive iterative (ne gere pas les sauts)
; !! > utiliser vm-run
(defun vm-run-old (vm &key (aff t))
  (vm-add-halt vm) ; ajoute HALT a la fin du code
  (loop for i from 0 to (vm-last-address vm)
        do
        (let (
              (bout-code (get-memory vm i))
             )
          (if (eq aff NIL)
              (vm-exec bout-code vm) ; sans affichage
            (format t "    ~28A  > ~8D~%" bout-code (vm-exec bout-code vm)) ; sinon avec affichage
          )
        )
  )
  (format t "~%>> Valeur de R0 :~%   ~A~%~%" (get-register vm 'R0))
)

; ok > (vm-run-old 'VM1 :aff NIL) ; sans affichage
; ok > (vm-run-old 'VM1) ; avec affichage pas a pas


; boucle infinie d'execution de la VM (en fait ne s'arrete qu'avec un HALT)
; on execute la case memoire qui est pointee par le PC
(defun vm-run (vm &key (aff t))
  (vm-add-halt vm) ; ajoute HALT a la fin du code
  (set-register vm 'PC (vm-main vm)) ; on fait pointer le PC sur le main
  (format t "~%>> La machine '~A' est en marche~%~%" vm)
  (loop while (not (equal (get-memory vm (get-register vm 'PC)) '(HALT)))
        do
        (let (
              (bout-code (get-memory vm (get-register vm 'PC)))
             )
          ; on ne fait rien s'il n'y a rien
          (if (eq bout-code NIL)
              ()
            ; sinon on execute le code
            (if (eq aff NIL)
                (vm-exec bout-code vm) ; sans affichage
              (format t "    ~28A  > ~8D~%" bout-code (vm-exec bout-code vm)) ; sinon avec affichage
            )
          )
        )
  )
  (format t "~%>> La machine '~A' est arretee~%" vm)
  (format t ">> Valeur de R0 : ~A~%~%" (get-register vm 'R0))
)

; ok > (vm-run 'VM1)


; cherche l'adresse main
; on cherche la premiere case qui ne fait pas parti d'un defun (entre ETIQ et RTN)
(defun vm-main (vm &optional (i 0))
  (let (
        (bout-code (get-memory vm i))
       )
    (cond ; si aucune instruction, main = 0
          ((eq bout-code ())
           (set-etiq vm 'main 0)
          )
          
          ; si c'est une etiquette, on cherche le premier RTN
          ((and (eq (cdr bout-code) NIL)  (not (eq (car bout-code) 'RTN)) (not (eq (car bout-code) 'NOP)) (not (eq (car bout-code) 'HALT)))
           (vm-main vm (+ (recherche vm '(RTN) i) 1)) 
          )
          
          ; si c'est un RTN, on teste avec le suivant
          ((eq (car bout-code) 'RTN)
           (vm-main vm (+ i 1)) 
          )
     
          ; sinon, on positionne le main et on sort
          (T (set-etiq vm 'main i))
    )
  )
)

; ok > (vm-main 'VM1)


; recherche un element dans la memoire
; retourne la position de la premiere occurrence cet element a partir du i
(defun recherche (vm elem &optional (i 0))
  (if (eq (get-memory vm i) ())
      ()
    (if (equal (get-memory vm i) elem)
        i
      (recherche vm elem (+ i 1))
    )
  )
)

; ok > (recherche 'VM1 '(RTN))


; pour lancer une fonction dont la definition a deja ete chargee en memoire
(defun vm-apply (fn vm &rest args)
  (if (not (get-etiq vm fn))
      (error "VM-APPLY : fonction '~A' non definie et/ou non chargee en memoire" fn)
    (progn (vm-load (compiler (append (list fn) args)) vm) ; on compile et on charge l'appel de la fonction avec tous ses args
           (vm-run vm) ; on lance la vm
    )
  )
)

; ok > (vm-apply 'carre 'VM1 3)



;*************************************************************************
; REGISTRES : liste de proprietes
; > (setf (get 'vm 'R0) 22) ; on stocke 22 dans le registre R0 de la VM
; > (get 'vm 'R0) ; on recupere ce qui est stocke dans le registre R0 de la VM
(defun set-register (vm reg val)
  (setf (get vm reg) val)
)

(defun get-register (vm reg)
  (get vm reg)
)

; pour afficher la valeur de tous les registres
(defun aff-register (vm)
  (format t "~%Etat des registres :~%********************~%  R0 : ~7D~%  R1 : ~7D~%  R2 : ~7D~%  SP : ~7D~%  FP : ~7D~%  BP : ~7D~%  PC : ~7D~%~%"
          (get-register vm 'R0)
          (get-register vm 'R1)
          (get-register vm 'R2)
          (get-register vm 'SP)
          (get-register vm 'FP)
          (get-register vm 'BP)
          (get-register vm 'PC)
  )
)


;*************************************************************************
; MEMOIRE : array
(defun set-memory (vm case-mem val)
  (setf (aref (get vm 'mem) case-mem) val)
)

(defun get-memory (vm case-mem)
  (aref (get vm 'mem) case-mem)
)

; pour affiche la memoire du code (bas de le memoire)
; exemple : (aff-memory-code 'VM1)
(defun aff-memory-code (vm)
(format t "~%Bas de la memoire (code) :~%**************************~%")
  (loop for i from 0 to (vm-last-address vm)
        do
        (format t "  ~6D  :  ~A~%" i (get-memory vm i))
  )
)


; pour affiche la memoire de la pile (haut de la memoire)
; exemple : (aff-memory-pile 'VM1)
(defun aff-memory-pile (vm)
(format t "~%Haut de la memoire (pile) :~%***************************~%")
  (loop for i from (vm-memory vm) downto (+ 1 (get-register vm 'SP))
        do
        (format t "  ~6D  :  ~A~%" i (get-memory vm i))
  )
)


;*************************************************************************
; ETIQUETTES : hashtable avec une adresse
(defun set-etiq (vm nom val)
  (setf (gethash nom (get vm 'etiq)) val)
)

(defun get-etiq (vm nom)
  (gethash nom (get vm 'etiq))
)


;*************************************************************************
; REFERENCES EN AVANT : hashtable avec une liste d'adresses
(defun set-ref (vm nom val)
  (setf (gethash nom (get vm 'ref)) val)
)

(defun get-ref (vm nom)
  (gethash nom (get vm 'ref))
)

; resoud les references en avant
(defun do-ref (vm nom val)
  (loop for ref in (get-ref vm nom)
        do ; pour chaque adresse de la liste des ref avant
        (let* (
               (bout-code (get-memory vm ref)) ; on recupere le code a traduire
               (saut (car bout-code)) ; type de saut : JMP, JSR, etc.
              )
          ; si un saut direct JMP ou JSR
          (if (or (eq saut 'JMP) (eq saut 'JSR))
              (set-memory vm ref `(,saut (MEM,val))) ; traduction
            ; sinon un saut de comparaison type JLE, JG, etc.
            (set-memory vm ref `(,saut ,(cadr bout-code) ,(caddr bout-code) (MEM,val)))
          )
        )
  )
  (remhash nom (get vm 'ref)) ; on supprime les ref en avant de cette etiq maintenant resolue
)


;*************************************************************************
; GESTION DES PARAMETRES
; (D 2) = #2 (valeur constante)
; (EAT X) = *@X (contenu d'une variable)
; (AT X) = @X (adresse d'une variable)
; R0 = registre R0
(defun get-param (vm val)
  (if (atom val)
      (get-register vm val) ; registre
    (cond ((or (eq (car val) 'D) (eq (car val) 'MEM)) (cadr val)) ; (D 2) ou (MEM 2) > 2
	  ((eq (car val) 'EAT) (get-memory vm (get-etiq vm (cadr val)))) ; EAT X = *@X
          ((eq (car val) 'AT) (get-etiq vm (cadr val))) ; AT X = @X
          ((eq (car val) 'FP) (get-memory vm (- (get-register vm 'FP) (- (cadr val) 1)))) ; FP + X - 1 (pour sauter le nb d'args) 
    )
  )
)

; ok > (get-param 'VM1 'R0)
; ok > (get-param 'VM1 '(D 33))
; ok > (get-param 'VM1 '(MEM 8))
; ok > (get-param 'VM1 '(EAT X))
; ok > (get-param 'VM1 '(FP -2))


;*************************************************************************
; INTERPRETE
; on analyse chaque element
(defun vm-exec (elem vm)
  (if (atom elem)
      ; mauvaise instruction, on passe a la suivante
      (vm-incr 'PC vm)
    (cond ; instructions de base
        ((eq (car elem) 'INCR) (vm-incr (cadr elem) vm) (vm-incr 'PC vm)) ; attention > (INCR PC) a indiquer hors de la def de vm-incr !
        ((eq (car elem) 'DECR) (vm-decr (cadr elem) vm))
        ((eq (car elem) 'MOVE) (vm-move (cadr elem) (caddr elem) vm))

        ; instructions de pile
        ; attention > pile = haut de la memoire
        ((eq (car elem) 'PUSH) (vm-push (cadr elem) vm))
        ((eq (car elem) 'POP) (vm-pop (cadr elem) vm))

	; instructions de saut
        ((eq (car elem) 'JMP) (vm-jmp (cadr elem) vm))
        ((eq (car elem) 'JSR) (vm-jsr (cadr elem) vm))
	((eq (car elem) 'RTN) (vm-rtn vm))
	((eq (car elem) 'JL) (vm-jl (cadr elem) (caddr elem) (cadddr elem) vm))
	((eq (car elem) 'JEQ) (vm-jeq (cadr elem) (caddr elem) (cadddr elem) vm))
	((eq (car elem) 'JG) (vm-jg (cadr elem) (caddr elem) (cadddr elem) vm))
	((eq (car elem) 'JLE) (vm-jle (cadr elem) (caddr elem) (cadddr elem) vm))
	((eq (car elem) 'JGE) (vm-jge (cadr elem) (caddr elem) (cadddr elem) vm))
	((eq (car elem) 'JNE) (vm-jne (cadr elem) (caddr elem) (cadddr elem) vm))

	; operateurs arithmetiques
	((eq (car elem) 'ADD) (vm-add (cadr elem) (caddr elem) vm))
        ((eq (car elem) 'SUB) (vm-sub (cadr elem) (caddr elem) vm))
	((eq (car elem) 'MULT) (vm-mult (cadr elem) (caddr elem) vm))
	((eq (car elem) 'DIV) (vm-div (cadr elem) (caddr elem) vm))

        ; instructions diverses
        ((eq (car elem) 'NOP) (vm-nop vm))
        ((eq (car elem) 'HALT) (vm-halt vm))

	; instruction inconnue
        (t (vm-halt vm))
    )
  )
)


;*************************************************************************
; VM-INCR
; (vm-incr dest vm) : on incremente une valeur
(defun vm-incr (dest vm)
  (if (atom dest) ; dest est un registre (cas general)
      (set-register vm dest (+ (get-param vm dest) 1))
    (cond
     ((eq (car dest) 'AT) ; dest est une adresse
      (set-etiq vm (cadr dest) (+ (get-param vm dest) 1))
     )
     ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
      (set-memory vm (get-etiq vm (cadr dest)) (+ (get-param vm dest) 1))
     )
    )
  )
)

; ok > (vm-incr 'R0 'VM1)
; ok > (vm-incr '(EAT X) 'VM1)


;*************************************************************************
; VM-DECR
; (vm-decr dest vm) : on decremente une valeur
(defun vm-decr (dest vm)
  (if (atom dest) ; dest est un registre (cas general)
      (set-register vm dest (- (get-param vm dest) 1))
    (cond
     ((eq (car dest) 'AT) ; dest est une adresse
      (set-etiq vm (cadr dest) (- (get-param vm dest) 1))
     )
     ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
      (set-memory vm (get-etiq vm (cadr dest)) (- (get-param vm dest) 1))
     )
    )
  )
)

; ok > (vm-decr 'R0 'VM1)
; ok > (vm-decr '(EAT X) 'VM1)


;*************************************************************************
; VM-MOVE
; (vm-move source dest vm) : on deplace source vers dest
; exemple :
;  source = 'R0 ou source = '(D 4)
;  dest = 'R1 ou dest = '(AT X)
(defun vm-move (source dest vm)
  (prog1 (if (atom dest) ; dest est un registre (cas general)
              (set-register vm dest (get-param vm source))
            (cond
               ((eq (car dest) 'AT) ; dest est une adresse
                (set-etiq vm (cadr dest) (get-param vm source))
               )
               ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
                (set-memory vm (get-etiq vm (cadr dest)) (get-param vm source))
               )
            )
         )
         (vm-incr 'PC vm)
  )
)

; ok > (vm-move '(D 3) 'R1 'VM1)
; ok > (vm-move 'R1 'R0 'VM1)


;*************************************************************************
; VM-PUSH
; (vm-push source vm) : on met le contenu de <source> sur le sommet de la pile
; Rappel > pile = haut de la memoire
; exemple : source = 'R0
(defun vm-push (source vm)
  (prog1 (set-memory vm (get-register vm 'SP) (get-param vm source)) ; on met la valeur la ou pointait SP > MOVE <source> SP
         (vm-decr 'SP vm) ; on incremente SP > en fait, on decrementre !
         (vm-incr 'PC vm)
  )
)

; ok > (vm-push 'R0 'VM1)
; ok > (vm-push '(D 3) 'VM1)


;*************************************************************************
; VM-POP
; (vm-pop dest vm) : on depile le sommet de la pile que l'on met a <dest>
; Rappel > pile = haut de la memoire
; exemple : dest = 'R0 (dest est un registre)
(defun vm-pop (dest vm)
  (if (<= (get-register vm 'BP) (get-register vm 'SP)) ; on a atteint le haut de la memoire (pile vide)
      (error "VM-POP : la pile est vide, impossible de depiler")
    (prog2 (vm-incr 'SP vm) ; on decremente SP > en fait, on incrementre !
           (set-register vm dest (get-memory vm (get-register vm 'SP))) ; on met le contenu de l'adr de SP dans <dest>
           (vm-incr 'PC vm)
    )
  )
)

; ok > (vm-pop 'R0 'VM1)


;*************************************************************************
; VM-ADD
; (vm-add source dest vm) : dest = dest + source
; dest est un registre mais source peut etre une constante ou une adresse
(defun vm-add (source dest vm)
  (prog1 (if (atom dest) ; dest est un registre (cas general)
              (set-register vm dest (+ (get-register vm dest) (get-param vm source)))
            (cond
               ((eq (car dest) 'AT) ; dest est une adresse
                (set-etiq vm (cadr dest) (+ (get-etiq vm (cadr dest)) (get-param vm source)))
               )
               ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
                (set-memory vm (get-etiq vm (cadr dest)) (+ (get-memory vm (get-etiq vm (cadr dest))) (get-param vm source)))
               )
            )
         )
         (vm-incr 'PC vm)
  )
)

; ok > (vm-add 'R1 'R0 'VM1)
; ok > (vm-add '(D 4) 'R0 'VM1)
; ok > (vm-add '(D 4) '(EAT X) 'VM1)


;*************************************************************************
; VM-SUB
; (vm-sub source dest vm) : dest = dest - source
(defun vm-sub (source dest vm)
  (prog1 (if (atom dest) ; dest est un registre (cas general)
              (set-register vm dest (- (get-register vm dest) (get-param vm source)))
            (cond
               ((eq (car dest) 'AT) ; dest est une adresse
                (set-etiq vm (cadr dest) (- (get-etiq vm (cadr dest)) (get-param vm source)))
               )
               ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
                (set-memory vm (get-etiq vm (cadr dest)) (- (get-memory vm (get-etiq vm (cadr dest))) (get-param vm source)))
               )
            )
         )
         (vm-incr 'PC vm)
  )
)


;*************************************************************************
; VM-MULT
; (vm-mult source dest vm) : dest = dest * source
(defun vm-mult (source dest vm)
  (prog1 (if (atom dest) ; dest est un registre (cas general)
              (set-register vm dest (* (get-register vm dest) (get-param vm source)))
            (cond
               ((eq (car dest) 'AT) ; dest est une adresse
                (set-etiq vm (cadr dest) (* (get-etiq vm (cadr dest)) (get-param vm source)))
               )
               ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
                (set-memory vm (get-etiq vm (cadr dest)) (* (get-memory vm (get-etiq vm (cadr dest))) (get-param vm source)))
               )
            )
         )
         (vm-incr 'PC vm)
  )
)


;*************************************************************************
; VM-DIV
; (vm-div source dest vm) : dest = dest / source
(defun vm-div (source dest vm)
  (if (= 0 (get-param vm source))
      (error "VM-DIV : erreur division par 0")
    (prog1 (if (atom dest) ; dest est un registre (cas general)
              (set-register vm dest (/ (get-register vm dest) (get-param vm source)))
             (cond
                ((eq (car dest) 'AT) ; dest est une adresse
                 (set-etiq vm (cadr dest) (/ (get-etiq vm (cadr dest)) (get-param vm source)))
                )
                ((eq (car dest) 'EAT) ; dest est la valeur contenue a une adresse
                 (set-memory vm (get-etiq vm (cadr dest)) (/ (get-memory vm (get-etiq vm (cadr dest))) (get-param vm source)))
                )
             )
           )
           (vm-incr 'PC vm)
    )
  )
)


;*************************************************************************
; VM-JMP
; (vm-jmp dest vm) 
; saute a l'adresse <dest>
; ie. MOVE <dest> PC
(defun vm-jmp (dest vm)
  (vm-move dest 'PC vm)
)


;*************************************************************************
; VM-JSR
; (vm-jsr etiq vm) 
; saute a l'adresse <etiq> et empile l'adresse de retour
; ie. PUSH PC
;     JMP @<etiq>
(defun vm-jsr (etiq vm)
  (vm-push 'PC vm)
  (vm-jmp etiq vm)
)


;*************************************************************************
; VM-JL
; (vm-jl val1 val2 dest vm) 
;  si val1 < val2, on saute a dest
;  sinon on incremente juste le PC
;  Attention > en fait, le contraire car les args sont inverses
(defun vm-jl (val1 val2 dest vm)
  (if (< (get-param vm val1) (get-param vm val2))
      (vm-incr 'PC vm)  
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-JG
; (vm-jg val1 val2 dest vm)
(defun vm-jg (val1 val2 dest vm)
  (if (> (get-param vm val1) (get-param vm val2))
      (vm-incr 'PC vm)
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-JLE
; (vm-jle val1 val2 dest vm)
(defun vm-jle (val1 val2 dest vm)
  (if (<= (get-param vm val1) (get-param vm val2))
      (vm-incr 'PC vm)
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-JGE
; (vm-jge val1 val2 dest vm)
(defun vm-jge (val1 val2 dest vm)
  (if (>= (get-param vm val1) (get-param vm val2))
      (vm-incr 'PC vm)
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-JEQ
; (vm-jeq val1 val2 dest vm)
(defun vm-jeq (val1 val2 dest vm)
  (if (= (get-param vm val1) (get-param vm val2))
      (vm-incr 'PC vm)
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-JNE
; (vm-jne val1 val2 dest vm)
(defun vm-jne (val1 val2 dest vm)
  (if (not (= (get-param vm val1) (get-param vm val2)))
      (vm-incr 'PC vm)
    (vm-jmp dest vm)
  )
)


;*************************************************************************
; VM-RTN
; (vm-rtn vm) : saute l'adresse du sommet de pile et le supprime
(defun vm-rtn (vm)
  (vm-pop 'R2 vm) ; un 2e POP car toutes nos operations terminent par un PUSH
  (vm-pop 'R2 vm) ; supprime le sommet de pile
  (vm-jmp 'R2 vm) ; saute vers l'ancien PC
  (get-register vm 'PC) ; affichage du PC
)


;*************************************************************************
; VM-NOP
; (vm-nop vm) : ne fait rien, on incremente juste le PC
(defun vm-nop (vm)
  (vm-incr 'PC vm)
)


;*************************************************************************
; VM-HALT
; (vm-halt vm) : arrete la VM
(defun vm-halt (vm)
  ; nouvelle version de HALT > ne fait plus rien !
  ; voir fin de boucle de vm-run ...
  ;(format t "~%>> La machine '~A' est arretee~%" vm)
  ;(format t ">> Valeur de R0 : ~A~%~%" (get-register vm 'R0))
  ;(break "HALT") ; pour tout arreter
)
