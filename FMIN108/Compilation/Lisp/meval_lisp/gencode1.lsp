;;GENERATEUR DE CODE;;;;;;;;;;;;;;;;;;;;;;;;
;on suppose que les valeurs de retour sont dans :r0 


(defparameter *code* nil)

;rajoute instr a la fin du code
(defun add(instr)
  (if (atom (car instr))
      (setf *CODE* (append *CODE* (list instr)))
    (setf *CODE* (append *CODE* instr))
    )
  )

(defun vm-compile (liste &optional env)
  (progn
    (setf *code* nil)
    (mcompile liste env)
    (add  '(HALT))
    )
  )
					
(defun mcompile (liste &optional env)
  (cond 
   ((atom liste) (mcoAtom liste env))
   ((equal (car liste) 'if) (mcoIf liste env))
   ((equal (car liste) 'quote) (mcoQuote (cdr liste) env))
   ((equal (car liste) 'progn) (mcoProgn (cdr liste) env))
   ((equal (car liste) 'defun) (mcoDefun (cdr liste) env))
   ((equal (car liste) 'mdefun) (mcoDefun (cdr liste) env))
   ((equal (car liste) 'lambda) (mcoLambda (cdr liste) env))
   ((equal (car liste) 'let) (mcoLet (cdr liste) env))
   ((listp (car liste)) (mcoevalArgs liste env) );forme ( f (args) )
   ((equal (car liste) 'setf) (mcoSetf (cdr liste) env))
   ((equal (car liste) 'equal) (mcoEqual (cdr liste) env))
   ((equal (car liste) 'cond) (mcoCond (cdr liste) env))
   ((member (car liste) '(+ - * / = > < <= >= /= ))  (mcoOp liste env))
   (`(function @,(car liste)) (mcoFn liste env))
   (T (error "pas fait")))
  )

(defun mcoAtom (liste &optional env)
  (if (constantp liste) (mcoCste liste);inclut t et nil
    (if (getEnv env liste) 
	(progn
	  (add  `(MOVE ,(list (getEnv env liste) :BP) :R0))
	  (add '(PUSH :R0))
	  )				
;sinon l'identificateur n'est pas delaredans l'env local
;il doit exister dans l'env global (gestion par la machine virtuelle)
      (progn
	(add `(MOVE (@ ,liste) :R0))
	(add '(PUSH :R0))
	)
      )
    )
  )

;empile les parametres effectifs  passes a une fonction ou un operateur
(defun pushargs (args taille env)
  (if (null args)
      (add `(PUSH ($ ,taille)))
    (progn
      (mcompile (car args) env)
      (pushargs (cdr args) (+ taille 1) env)
      )
    )
  )
;idem que ci-dessus mais adapteau let (couples)
(defun pushargsLet (args taille env)
  (if (null args)
      (add `(PUSH ($ ,taille)))
    (progn 
      (mcompile (cadar args) env)
      (pushargsLet (cdr args) (+ taille 1) env)
      )
    )
  )
;regroupe les opeateurs
(defun mcoOp (liste &optional env )
  (progn
    (pushargs (cdr liste) 0 env)
    (add (case (car liste)
	   (+ (mcoOp+))
	   (- (mcoOp-))
	   (* (mcoOp*))
	   (/ (mcoOp/))
	   (/= (mcoOp/=))
	   (T (mcoOpTest (car liste)))
	   ) 
	 )
    )
  )

(defun mcoFn (liste &optional env )
  (progn
    (pushargs (cdr liste) 0 env)
    (add `(JSR (@ ,(car liste))))
    (add '(POP :R1));depilage des parametres
    (add '(SUB :SP :R1))
    (add '(MOVE :R1 :SP))
    (add '(PUSH :R0))
    )
  )

(defun mcoProgn (liste &optional env)
  (labels (
	   (evallist (lst)
		     (if (null (cdr lst))
			 (mcompile (car lst) env)
		       (progn
			 (mcompile (car lst) env)
			 (add '(POP :R2))
			 (evallist (cdr lst))
			 )
		       )
		     )
	   )
    (evallist  liste)
    )
  )
;les operateurs de base peuvent avoir un nombre quelconque
;d'arguments, cela explique la grosse taille du code gememe pour
;un operateur appliquea 2 arguments 
;les arguments sont dans la pile (ordre inverse)
;en sommet de pile se trouve le nombre d'arguments
(defun mcoOp+()
  (let (
	(lbl (gensym "LBL_"))
	(fin (gensym "END_"))
	(fin0 (gensym "END0_"))
	)
    `( 
      (POP :R2)
      (JEQ :R2 (@ ,fin0))
      ( @ ,lbl )
      (DECR :R2)
      (JEQ :R2 (@ ,fin))
      (POP :R0 )
      (POP :R1)
      (ADD :R1 :R0)
      (PUSH :R0)
      (JMP (@ ,lbl))
      (@ ,fin0)
      (PUSH ($ 0))
      (@ ,fin)
      )
    )
  )

(defun mcoOp*()
  (let (
	(lbl (gensym "LBL_"))
	(fin (gensym "END_"))
	(fin1 (gensym "END1_"))
	)
    `( 
      (POP :R2)
      (JEQ :R2 (@ ,fin1))
      ( @ ,lbl )
      (DECR :R2)
      (JEQ :R2 (@ ,fin))
      (POP :R0 )
      (POP :R1)
      (MULT :R1 :R0)
      (PUSH :R0)
      (JMP (@ ,lbl))
      (@ ,fin1)
      (PUSH ($ 1))
      (@ ,fin)
      )
    )
  )

(defun mcoOp-()
  (let (
	(fin (gensym "END_"))
	(fin0 (gensym "END0_"))
	(erreur (gensym "ERR_"))
	)
    `(
      (POP :R2)
      (JEQ :R2 (@ ,erreur))
      (DECR :R2)
      (JEQ :R2 (@ ,fin0))
      (PUSH :R2)
      ,@(mcoOp+)
      (POP :R0)
      (POP :R1)
      (SUB :R1 :R0)
      (PUSH :R0)
      (JMP (@ ,fin))
      (@ ,erreur)
      (error "trop peu de parametres")
      (JMP (@ ,fin))
      (@ ,fin0)
      (POP :R0)
      (MOVE ($ 0) :R1)
      (SUB :R1 :R0)
      (PUSH :R0)
      (@ ,fin)
      )
    )
  )

(defun mcoOp/()
  (let (
	(fin (gensym "END_"))
	(fin1 (gensym "END1_"))
	(erreur (gensym "ERR_"))
	)
    `(
      (POP :R2)
      (JEQ :R2 (@ ,erreur))
      (DECR :R2)
      (JEQ :R2 (@ ,fin1))
      (PUSH :R2)
      ,@(mcoOp*)
      (POP :R0)
      (POP :R1)
      (DIV :R1 :R0)
      (PUSH :R0)
      (JMP (@ ,fin))
      (@ ,erreur)
      (error "trop peu de parametres")
      (JMP (@ ,fin))
      (@ ,fin1)
      (POP :R0)
      (MOVE ($ 1) :R1)
      (DIV :R1 :R0)
      (PUSH :R0)
      (@ ,fin)
      )
    )
  )

;tous les opeateurs de test fonctionnent de la meme facon
;et ils sont multiples (nombre quelconque d'arguments)
(defun mcoOpTest(op)
  (let (
       	(lbl (gensym "LBL_"))
       	(fin (gensym "END_"))
     	(vraie (gensym "VRAI_"))
	(faux (gensym "FAUX_"))
	(zero (gensym "ZERO_"))
	)
    `(
      (POP :R2)
      (JEQ :R2 (@ ,zero))
      (DECR :R2)
      (JNE :R2 (@ ,lbl))
      (POP :R1)
      (JMP (@ ,vraie))
      (@ ,lbl)
      (POP :R1)
      (JEQ :R2 (@ ,vraie))
      (DECR :R2)
      (POP :R0)
      (PUSH :R0);on doit sauvegarder cette valeur
      (SUB :R0 :R1)
      ,(case op
	 ( <  `(JL :R1 (@ ,lbl)))
	 ( <=  `(JLE :R1 (@ ,lbl)))
	 ( >  `(JG :R1 (@ ,lbl)))
	 ( >=  `(JGE :R1 (@ ,lbl)))
	 ( =  `(JEQ :R1 (@ ,lbl)))
	 )
      (@ ,faux)
      (DECR :R2);faire plutot un depilage par rapport a SP et :R2
      (POP :R1)
      (JGE :R2 (@ ,faux))
      (MOVE ($ nil) :R0)
      (PUSH :R0)
      (JMP (@ ,fin))
      (@ ,zero)
      (erreur "pas assez d'args")
      (@ ,vraie)
      (MOVE ($ t) :R0)
      (PUSH :R0)
      (@ ,fin)
      )
    )
  )


(defun mcoOp/= ()
  (let (
	(lbl (gensym "LBL_"))
	(fin (gensym "END_"))
	)
    `(
      ,@(mcoOpTest '=)
      (POP :R0)
      (JEQ :R0 (@ ,lbl))
      (MOVE ($ 0) :R0)
      (JMP (@ ,fin))
      (@ ,lbl)
      (MOVE ($ 1) :R0)
      (@ ,fin)
      (PUSH :R0)
      )
    )
  )

;le cas du nil posait un probleme car les fonctions de test
;dont nous disposions ne comparaient un registre que par rapport 
;on a alors rajouteune instruction pour comparer 2 registres entre eux
(defun mcoIf(liste &optional env)
  (let (
	(else (gensym "ELSE_"))
	(end (gensym "END_"))
	)
    (progn
      (mcompile (cadr liste) env);evalue le test
      (add '(POP :R1))
      (add `(JEQUAL :R1 ($ nil ) (@ ,else)))
      (mcompile (caddr liste) env);evalue le then
      (add `(JMP (@ ,end)))
      (add `(@ ,else))
      (mcompile (cadddr liste) env);else
      (add `(@ ,end) )
      )
    )
  )

(defun mcoCste(cste)
  (add	 `(MOVE ($ ,cste) :R0))
  (add	'(PUSH :R0))
  )

;;;;;;;;gestion de l'environnement
(defun addEnv (env symb val) ;Ajoute (symb val) au debut de env
  (push (list symb val) env)
  )

(defun getEnv (env symb) ;Renvoie val si (symb val)
					; est dans env, sinon revoie nil
  (if (null env)
      nil
    (if (equal symb (car (car env)))
	(car (cdr (car env)))
      (getEnv (cdr env) symb)
      )
    )
  )

(defun mcoCond (liste &optional env)
  (let ((sortie (gensym "out")))
    (labels (
	     (lirecond (lst)
		       (if (null (cdr lst))
			   (progn;le dernier couple ((cond) (instr))
			     (mcompile (caar lst) env)
			     (add '(POP :R1))
			     (add `(MOVE ($ nil) :R2))
			     (add `(JEQUAL :R2 :R0 (@ ,sortie)));si test vaut nil, out
			     (mcompile (cadar lst) env)
			     (add '(POP :R1))
			     )
			 (progn
			   (let ((suivant (gensym "cond")))
			     (progn
			       (mcompile (caar lst) env);evalue le test
			       (add '(POP :R1))
			       (add `(MOVE ($ nil) :R2))
			       (add `(JEQUAL :R2 :R0 (@ ,suivant)));si test vaut nil, va au suivant
			       (mcompile (cadar lst) env);evalue le corps si ok
			       (add '(POP :R1))
			       (add `(JMP (@ ,sortie)));puis sort
			       (add `(@ ,suivant))
			       )
			     )
			   (lirecond (cdr lst))
			   )
			 )
		       )
	     )
      (progn
	(lirecond  liste)
	(add `(@ ,sortie))
	(add `(PUSH :R0))
	)
      )
    )
  )

;on vire le defun a l'appel 
(defun mcoDefun (liste &optional env)
  (add `(@ ,(car liste)))
  (add `(PUSH :BP))
  (add `(MOVE :SP :BP))
;rajoute a l'env les parametre du defun 
;params : la liste des parametres formels dans l'ordre du defun , ex (x y)
;index est le decalage par rapport a BP (ici -4)
  (progn
    (pushparams (cadr liste) (- -3 (length (cadr liste)))  env)
    (mcompile (caddr liste) (pushparams (cadr liste)(- -3 (length (cadr liste)))  env))
    (add `(MOVE :BP :SP))
    (add `(POP :BP));cf  mv, on restaure BP
    (add `(RTN))
    )
  )

(defun mcoLambda (liste &optional env)
  (progn
    (add `(PUSH :BP))
    (add `(MOVE :SP :BP))
    (pushparams (cadr liste) (- -3 (length (cadr liste)))  env)
    (mcompile (cadr liste) ;traite l'env
	      (pushparams (car liste) (- -3 (length (car liste)))  env))
    (add `(MOVE :BP :SP))
    (add `(POP :BP));cf  mv, on restaure BP
    (add `(RTN))
    )
  )

;applique la fonction definie entre parentheses aux arguments hors de la
;parenthese : typiquement ((lambda ...) args )
(defun mcoevalArgs (liste &optional env)
  (let ((corps (gensym "FN"))
	(fin (gensym "SUBR")))
    (progn
      (pushargs (cdr liste) 0 env)
      (add `(JSR (@ ,corps)))
      (add `(JMP (@ ,fin))) 
      (add `(@ ,corps)) 
      (mcompile (car liste) (pushparams (cadar liste) (- -3 (length (cadar liste)))  env))
      (add `(@ ,fin))
      (add '(POP :R1));depilage des parametres
      (add '(SUB :SP :R1))
      (add '(MOVE :R1 :SP))
      (add '(PUSH :R0))
      )
    )
  )

(defun mcoQuote (liste &optional env)
  (add `(PUSH ($ ,@liste)))
  )

;ne marche que si la pile na pas ete modifiee depuis l'entree
;dans le stack frame courant ....
(defun mcoLet (liste &optional env)
  (let (
	(longueur (length (car liste)))
	)
    (progn
      (pushargsLet (car liste) 0 env)
      (add '(POP :R1));voir pushargs...
      (mcompile
       (cadr liste)
       (pushparamsLet  (car liste) 0 env);place les symboles, adr dans l'env
       )
      (add `(MOVE ($ ,(+ longueur 1)) :R1))
      (add '(SUB :SP :R1))
      (add '(MOVE :R1 :SP))
      (add '(PUSH :R0))
      )
    )
  )

;evalue le membre droit de l'affectation
;dans tous les cas modifie la liaison associee au  symbole dans l'env global 
;le rajout ou simplement la modif sont gere par la machine virtuelle
;dans le cas ou le symbole existe dans l'environnement local on ne modifie
;que la premiere occurrence rencontree
(defun mcoSetf (liste &optional env)
  (mcompile (cadr liste) env)
  (add `(MOVE :R0 (@ ,(car liste))))
  (if (getEnv env (car liste))
      (add `(MOVE :R0 ,(list (getEnv env (car liste)) :BP)))
    )
  )

(defun mcoEqual (liste &optional env)
  (let (
       	(fin (gensym "END_"))
    (vraie (gensym "VRAI_"))
    (badna (gensym "BADA_"))
    )
    (progn
      (pushargs  liste 0 env)
      (add
       `(
	 (POP :R1);nb d'arguments (cf pushargs)
	 (MOVE ($ 2) :R2)
	 (SUB :R2 :R1)
	 (JNE :R1 (@ ,badna))
	 (POP :R1)
	 (POP :R2)
	 (JEQUAL :R1 :R2 (@ ,vraie))
	 (MOVE ($ nil) :R0)
	 (JMP (@ ,fin))
	 (@ ,badna)
	 (erreur "Bad Number Of Args")
	 (HALT)
	 (@ ,vraie)
	 (MOVE ($ t) :R0)
	 (@ ,fin)
	 (PUSH :R0)
	 )
       )
      )
    )
  )

;ajoute dans l'environnement la liste d'associations (vars, decalage)
;utiliseous deux formes :
;pour les  parametresformels  de fonctions
;pour les variables  locales
;index represente le decalage (adressage indexe ) a appliquer a BP
;pour avoir l'adresse de la valeur correspondant au symbole 
(defun pushparams (params index env)
  (if (null params)
      env
    (pushparams (cdr params)
		(+ index 1)
		(addEnv env (car params) index)
		)
    )
  )

(defun pushparamsLet (params index env)
  (if (null params)
      env
    (pushparamsLet (cdr params)
		   (+ index 1)
		   (addEnv env (caar params) index)
		   )
    )
  )

