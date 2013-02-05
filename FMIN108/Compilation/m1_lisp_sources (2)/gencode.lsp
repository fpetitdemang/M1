#|

  TER 141 2005-2006
  Generation de code
  gencode.lsp

  Lebec Laurie : laurie.lebec@gmail.com
  Leture Julien : julien@leture.com

|#


; (load "~/Documents/Cours/Lisp/TER 141/gencode.lsp")


;*************************************************************************
; A FAIRE
;  compiler-let
;  compiler-loop


;*************************************************************************
; COMPILER
; fonction de compilation
; analyse par cas
(defun compiler (expr &optional env)
  ; si une expression atomique
  (if (atom expr)
      (cond ; soit un nombre
          ((numberp expr)
          `((MOVE (D,expr) R0) ; (D 2) = #2
            (PUSH R0))
          )
	  ; soit une variable qui n'est pas dans l'environnement (variable globale)
          ((eq (getvar expr env) NIL) ; ie. pas l'env
           `((MOVE (EAT,expr) R0) ; (EAT X) = *@X
             (PUSH R0))
          )
          ; soit une variable qui est dans l'environnement (variable locale)
          (T `((MOVE (FP ,(getvar expr env)) R0) ; lors de la compilation des arguments d'un defun
               (PUSH R0))
          )
      )

    ; sinon c'est une liste
    (cond ; si un operation arithmetique
     ((eq (car expr) '+) (compiler-operation 'ADD expr env))
     ((eq (car expr) '-) (compiler-operation 'SUB expr env))
     ((eq (car expr) '*) (compiler-operation 'MULT expr env))
     ((eq (car expr) '/) (compiler-operation 'DIV expr env))

          ; si une comparaison dans un test avec une etiquette
          ; si une comparaison simple, voir appel de fonction avec (JSR (AT >))
     ((eq (car expr) '<) (compiler-comp 'JL expr env))
     ((eq (car expr) '=) (compiler-comp 'JEQ expr env))
     ((eq (car expr) '>) (compiler-comp 'JG expr env))
     ((eq (car expr) '<=) (compiler-comp 'JLE expr env))
     ((eq (car expr) '>=) (compiler-comp 'JGE expr env))
     ((eq (car expr) '!=) (compiler-comp 'JNE expr env))

          ; si un operateur LISP
     ((eq (car expr) 'quote) (compiler-readlisp expr env))
     ((eq (car expr) 'cons) (compiler-readlisp expr env))
     ((eq (car expr) 'append) (compiler-readlisp expr env))

     ((eq (car expr) 'progn) (compiler-readlisp expr env))
     ((eq (car expr) 'prog1) (compiler-readlisp expr env))
     ((eq (car expr) 'prog2) (compiler-readlisp expr env))

     ((eq (car expr) 'car) (compiler-readlisp expr env))
     ((eq (car expr) 'cdr) (compiler-readlisp expr env))
     ((eq (car expr) 'cadr) (compiler-readlisp expr env))
     ((eq (car expr) 'cddr) (compiler-readlisp expr env))
     ((eq (car expr) 'caddr) (compiler-readlisp expr env))
     ((eq (car expr) 'cdddr) (compiler-readlisp expr env))
     ((eq (car expr) 'cadddr) (compiler-readlisp expr env))
     ((eq (car expr) 'cddddr) (compiler-readlisp expr env))

          ; si un if
     ((eq (car expr) 'if) (compiler-if (cdr expr) env))

          ; si un cond, on le transforme en if
     ((eq (car expr) 'cond) (compiler (cond2if (cdr expr)) env))

          ; si un setf
     ((eq (car expr) 'setf) (compiler-setf expr env))

          ; si un let
          ; Attention > compiler-let existe deja en lisp
     ;((eq (car expr) 'let) (compiler-llet expr env))

          ; si un loop
     ;((eq (car expr) 'loop) (compiler-loop expr env))

          ; si un defun
     ((eq (car expr) 'defun) (compiler-defun (cdr expr) env)) 
     
          ; sinon appel a une fonction
     (T (compiler-appel expr env))
   )
  )
)

; ok > (compiler '3)
; ok > (compiler 'X '((X . 1)(Y . 2)))
; ok > (compiler '(+ 2 3))
; ok > (compiler '(+ (* 5 2) (+ 2 (- 2 3))))
; ok > (compiler '(< 2 4 toto))
; ok > (compiler '(if (< a b) c d))
; ok > (compiler '(setf a 18))
; ok > (compiler '(toto 1 2 3))
; ok > (compiler '(toto X Y Z) '((X . -1)(Y . -2)(Z . -3)))
; ok > (compiler '(defun toto (a b c d) (if (< a b) c d)))


;*************************************************************************
; fonction pour compiler une fonction LISP predefinie
; exemple :
;  expr = (car '(1 2 3))
;  env = ()
(defun compiler-readlisp (expr env)
  (let (
        (val (eval expr)) ; eval > merci lisp
       )
    (compiler val env)
  )
)

; > (compiler-readlisp '(car '(1 2 3)) ())
; > (compiler-readlisp '(car '(A B C)) ((A . 3)(B . 4)(C . 5)))


;*************************************************************************
; fonction de compilation lors de l'appel d'une fonction
; exemple :
;  appel = '(toto X Y)
;  env = '((X . 1)(Y . 2)))
(defun compiler-appel (appel env)
  (let (
	(etiq (car appel))
	(nb (length (cdr appel))) ; compteur = longueur liste arguments
       )
    (append (compiler-args (cdr appel) env)
	    `((PUSH (D,nb)) ; on pousse le nombre d'arguments
	      (MOVE FP R1) ; on sauve l'ancien FP
	      (MOVE SP FP) ; nouvel FP = SP
	      (MOVE SP R2) ; on sauve l'ancien SP
	      (ADD (D,nb) R2) ; et non (SUB (D,nb) R2) car la pile descend
	      (ADD (D 1) R2) ; et non (SUB (D 1) R2) car la pile descend
	      (PUSH R2) ; on sauve l'ancien SP
	      (PUSH R1) ; on sauve l'ancien FP
	      (JSR (AT,etiq)) ; appel de la fonction
	      (POP R1) ; restauration FP
	      (POP R2) ; restauration SP
	      (MOVE R1 FP)
	      (MOVE R2 SP)
              (PUSH R0)
	      )
    )
  )
)

; ok > (compiler-appel '(toto 1 2) ())
; ok > (compiler-appel '(toto X Y) '((X . 1)(Y . 2)))
; ok > (compiler-appel '(toto 1 2 (+ 3 4)) ())
; ok > (compiler-appel '(toto 1 2 (titi 3 4)) ())


;*************************************************************************
; compile les args
; exemple :
;  largs = '(1 2 3 4)
;  env = ()
(defun compiler-args (largs env)
  (if (null largs)
      ()
    (append (compiler (car largs) env)
            (compiler-args (cdr largs) env)
    )
  )
)

; ok > (compiler-args '(1 2 3 4) ())
; ok > (compiler-args '(X Y) '((X . 1)(Y . 2)))


;*************************************************************************
; retourne la valeur associee a une variable pour un environnement donne
; > (getvar 'X '((Y . 2) (X . 1) (Z . 3)))
; > 1
(defun getvar (arg env)
  (let (
	(cell (assoc arg env))
        )
    (if cell
	(cdr cell)
      ()
    )
  )
)


;*************************************************************************
; fabrique un environnement
; exemple :
;  largs = '(X Y Z)
;  env = ()
;  nb-args = 3
(defun make-env (largs env nb-args)
  (if (null largs)
      ()
    (cons (cons (car largs) (- 0 nb-args)) ; pour ecrire -3 a partir de 3
	  (make-env (cdr largs) env (- nb-args 1)))
    )
)

; ok > (make-env '(X Y Z) () 3)
; >>>> ((X . -3) (Y . -2) (Z . -1))


;*************************************************************************
; pour compiler un defun (definition de fonction)
; exemple :
;  code = '(toto (a b c d) (if (< a b) c d))
;  env = ()
(defun compiler-defun (code env)
  (let (
	(nenv (make-env (cadr code) env (length (cadr code)))) ; creation du nouvel environnement
	(etiq (car code))
	)
    (append `((,etiq))
	    (compiler (caddr code) nenv)
	    `((RTN))
    )
  )
)

; ok > (compiler-defun '(toto (a b c d) (if (< a b) c d)) ())


;*************************************************************************
; pour compiler un if
; (<test> <alors> <sinon>)
; exemple :
;  code = '((< a b) (toto a) (titi b))
;  env = ()
(defun compiler-if (code env)
  (let (
	(etiq-sinon (cpt-incr))
	(etiq-fin (cpt-incr))
       )
    (append (compiler (append (car code) (list etiq-sinon)) env) ; on compile le test en y integrant l'etiquette du sinon
	    (compiler (cadr code) env) ; si vrai, on compile le <alors>
	    `((JMP (AT,etiq-fin))
	      (,etiq-sinon))
	    (compiler (caddr code) env) ; on compile le <sinon>
	    `((RTN)
              (,etiq-fin))
    )
  )
)

; ok > (compiler-if '((< a b) (toto a) (titi b)) ())
; ok > (compiler-if '((< a 5) 1 2) ())


;*************************************************************************
; pour transformer un cond en if
(defun cond2if (expr)
  (if (null expr)
      ()
    (if (eq (caar expr) 't) ; cas du T final
        (cadar expr)
      `(if,(caar expr) ; cas general
	  ,(progn (cadar expr))
	  ,(cond2if (cdr expr))
       )
    )
  )
)

; ok > (cond2if '(((= a 0) b) ((< a 0) c) ((> a 0) d)))
; >>>> ie. (cond ((= a 0) b)
;                ((< a 0) c)
;                ((> a 0) d))

; ok > (cond2if '(((= a 0) (b1 b2)) ((< a 0) c) ((> a 0) d) (t e)))
; >>>> ie. (cond ((= a 0) (b1 b2))
;                ((< a 0) c)
;                ((> a 0) d)
;                (t e))


;*************************************************************************
; pour compiler un let > ajoute les variables dans l'env local
; Attention > compiler-let est deja une fonction LISP
; exemple :
;  code = '
;  env = ()
#|
(defun compiler-llet (code env)
  (append env(cadr code) env)
  (compiler (caddr code) env)
)  
|#
; > (compiler-llet '(let ((x 1)) x) ())


;*************************************************************************
; pour compiler un setf
; exemple :
;  code = '(setf a 18)
;  env = ()
(defun compiler-setf (code env)
  (append (compiler (caddr code) env)
	  `((MOVE R0 (EAT,(cadr code))))
  )
)

; ok > (compiler-setf '(setf a 18) ())
; ok > (compiler-setf '(setf a (+ a 5)) ())


;*************************************************************************
; compile une operation
; exemple :
;  op = 'ADD
;  expr = '(+ 2 3)
;  env = ()
(defun compiler-operation (op expr env)
  (append (compiler (cadr expr) env)
          (compiler (caddr expr) env)
          `((POP R1)
            (POP R0)
            (,op R1 R0)
            (PUSH R0))
  )
)

; ok > (compiler-operation 'ADD '(+ 2 3) ())


;*************************************************************************
; compile une comparaison
; exemple :
;  op = 'JL
;  expr = '(< 2 3 toto) > l'etiq se trouve a la fin de l'expr
;  env = ()
(defun compiler-comp (op expr env)
  (append (compiler (cadr expr) env)
          (compiler (caddr expr) env)
          `((POP R0)
            (POP R1)
            (,op R1 R0 (AT,(cadddr expr))))
  )
)

; ok > (compiler-comp 'JL '(< 2 3 toto) ())


;*************************************************************************
; compteur
; pour generer des etiquettes numeriques uniques
(let ((cpt 0))
   (defun cpt-start () cpt)
   (defun cpt-reset () (setf cpt 0))
   (defun cpt-incr () (setf cpt (+ cpt 1)))
   (defun cpt-decr () (setf cpt (- cpt 1)))
)


;*************************************************************************
; affiche du code ligne par ligne
; exemple : (affiche (compiler '(toto 1 2 (titi 3 4))))
(defun affiche (code)
  (if (null code)
      ()
    (progn (print (car code))
           (affiche (cdr code))
    )
  )
)


;*************************************************************************
; ouvre et compile un fichier
; exemple : (compiler-fichier "~/Desktop/test.lsp")
(defun compiler-fichier (fichier &key (aff t))
  (let (
        (flux (open fichier))
       )
    (loop until (not (listen flux))
          do
          (let (
                (code (read flux NIL)) ; NIL pour enlever le warning du eof
               )
            (format t "~%~%On va compiler le code suivant :~%~A~%" code)
            (if (eq aff NIL) ; si aff est vide on affiche sous forme de liste
                (format t "~%Resultat :~%~A~%" (compiler code))
              (affiche (compiler code)) ; sinon une instruction par ligne
	    )
          )
    )
    (close flux)
  )
)
