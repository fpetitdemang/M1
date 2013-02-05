;//-------------------------------------------------------// 
;//-------------------------------------------------------// 
;//         META-EVALUATEUR  -->  Tp Lisp 2007/2008       //
;//-------------------------------------------------------// 
;//-------------------------------------------------------// 

;//
;// META-EVALUATEUR DE BASE
;// 
(defun meval (expr &optional env fenv)
  (cond 
   
   ;1 -> constante atomique
   ((and (atom expr) (constantp expr))
    expr)
   
   ;2 -> constante non atomique, donc variable
   ((atom expr)
    (let ((cell (assoc expr env)))
      (if cell
	  (cdr cell)
	(error "~s n'est pas une variable" expr))))

   ;3 -> plus d'atome à partir d'ici
   ((and
     (consp (car expr))
     (eq 'lambda (caar expr)))
    (meval-lambda (car expr)
		  (meval-args (cdr expr) env) env))
    
   ;4 -> Une fonction et un symbole non constant   
   ((or (not (symbolp (car expr))) (constantp (car expr)))
    (error "~s can't be a function" expr))

   ;5 -> Une fonction locale
   ((assoc (car expr) fenv)
    (meval-closure (cdr (assoc (car expr) fenv))
		   (meval-args (cdr expr) env fenv)))

   ;6 -> Une fonction globale
   ((get-defun (car expr))
    (meval-closure (get-defun (car expr)) (meval-args (cdr expr) env)))
    
   ;7 -> macro meta-definie
   ((get-defmacro (car expr))
    (meval (meval-lambda (get-defmacro (car expr)) (cdr expr) ())
	   env))
   
   ;8 -> defun
   ((eq 'defun (car expr))
    (set-defun (cadr expr)
	       (make-closure `(lambda ,@(cddr expr)) env)))

   ;9 -> macro
   ((eq 'defmacro (car expr))
    (set-defmacro (cadr expr) `(lambda ,@(cddr expr))))

   ;10 -> quote
   ((and (consp expr) (eq 'quote (car expr)))
    (cadr expr))
   
   ;11 -> setf : affectation 
   ((eq 'setf (car expr))
    (msetf (cadr expr) (caddr expr) env))

   ;12 -> flet : une fonction locale non recursive
   ((eq 'flet (car expr))
    (meval-body (cddr expr)
		env
		(make-flet-env ( cadr expr ) env fenc)))

   ;13 -> labels : une fonction locale recursive
   ((eq 'labels (car expr))
    (meval-body (cddr expr)
		env
		(make-labels-env ( cadr expr ) env fenc)))

   ;14 -> progn 
   ((eql 'progn (car expr))
    (meval-body (cdr expr) env fenv))
       
   ;15 -> :closure
   ((eql (car expr) `:closure) 
    expr) 

   ;16 -> cond
   ((eql 'cond (car expr))
    (meval-cond (cdr expr) env fenv))
      
   ;17 -> let
   ((or (eql 'let (car expr))
	(eql 'let* (car expr)))
    (meval-let (cdr expr) env fenv)) 
   
   ;18 -> time
   ((eql 'time (car expr)) 
    (time (meval (cadr expr) env fenv)))
   
   ;19 -> load
   ((eql 'load (car expr))
    (mload (cadr expr)))

   ;20 -> function
   ((eq 'function (car expr))
    (cond ((consp (cadr expr))
	   (make-closure (cadr expr) env))
	  ((get-defun (cadr expr)))
	  (t (symbol-function (cadr expr)))))

   ;21 -> apply
   ((eql (car expr) 'apply)
    (let ((fun (meval (cadr expr) env fenv)))
      (if (and (consp fun) (eql (car fun) :closure))
	  (meval-closure fun (apply #'list* (meval-args (cddr expr) env fenv)))
      	(apply fun  (apply #'list* (meval-args (cddr expr) env fenv))) )))

   ;22 -> if
   ((eq 'if (car expr))
    (if (meval (cadr expr) env)
	(meval (caddr expr) env)
      (meval (cadddr expr) env)))
    
   ;23 -> forme speial non traite
   ((not(fboundp (car expr)))
    (error "~s not defined" (car expr)))
   
   ;24 -> macro pr��inie
   ((macro-function (car expr))
    (meval (displace expr (macroexpand-1 expr)) env))
    
   ;25 -> ramasse miette
   ((special-form-p (car expr))
    (if (null env)
	(eval expr)
      (error "~s special form not yet implemented" (car expr))))
   
   ;26 -> fonction globale
   (t
    (apply (symbol-function (car expr)) (meval-args (cdr expr) env)))
   ))


;//
;// META-EVALUATEUR TRANS
;// 
(defun meval-trans (exp &optional env fenv)
  (progn
    (print exp)
    (case (car exp) 
	  
	  ((:const)     (cdr exp))
	  
	  ((:var)       (list 'aref env (cdr exp)))
	  
	  ((:set-var)   (list 'aref env (list 'cadr exp))) 
	  
	  ((:set-cvar)  (set-array (cadr exp) (caddr exp) (meval (cdddr exp) env fenv) env))
	  
	  ((:quote)     (cadr exp))
	  
	  ((:closure)   exp) 
	  
	  ((:lcall)     (meval-trans-closure (cons ':closure (meval-local-function (cadr exp) fenv)) (meval-trans-liste  (cddr exp) env fenv)))
          
	  ((:special)   (meval  (macroexpand-1 exp) env fenv)) 
	  
	  ((:mcall)     (meval-trans-closure (get (cadr exp) ':defun) (meval-trans-liste  (cddr exp) env fenv)))
	  
	  ((:vcall)     (if (> (length (cddr exp)) 1)
			    (cons (cadr exp) (meval-trans-liste (quotifier (caddr exp)) env fenv))
			  `( ,(cadr exp) ,(meval-trans (quotifier (caddr exp)) env fenv))))
	  
	  ((eql (car exp) 'system::backquote)  exp)
	  
	  (t
	   (error "type inconnue")))))


;FONCTION ANNEXE DU MEVAL-TRANS
(defun meval-trans-liste (exp &optional env fenv)
  (if (atom exp)
      nil
    (if (null (cdr exp))
	(list (meval-trans (car exp) env fenv))
      (progn
	(list* (meval-trans (car exp) env fenv)
	       (meval-trans-liste (cdr exp) env fenv))))))

(defun meval-local-function (nom &optional env fenv)
  (if (null fenv)
      nil
    (if (and (consp (car fenv)) (eql nom (caar fenv)))
        (cdr (car fenv))
      (meval-local-function nom env (cdr fenv)))))

(defun quotifier (exp)
  (if (atom exp)
      exp
    (if (and 
	 (consp (car exp))
	 (eql 'quote (caar exp)))
	(cons (list 'quote (car exp)) (quotifier (cdr exp)))
      (cons (car exp) (quotifier (cdr exp))))))

;FONCTION ANNEXE DU MEVAL -> MEVAL-LET
(defun meval-let (exp &optional env fenv)
  (meval-body (cdr exp) (meval-let-aux (car exp) env fenv) fenv))

(defun meval-let-aux (l &optional env fenv)
  (if (atom l)
      env
    (if (and (consp (car l)) (eql (length (car l)) 2))
	(cons (cons (caar l) (meval (cadar l) env fenv)) (meval-let-aux (cdr l) env fenv))
      nil)))


;FONCTION ANNEXE DU MEVAL -> MEVAL-COND
(defun meval-cond (exp &optional env fenv)
  (cond ((null exp) nil)
        ((atom (car exp)) (meval (car exp) env fenv) )
        (t (let ((val (meval (caar exp) env fenv)))
	     (if val
		 (if (null (cdar exp))
		     val
		   (meval-body (cdar exp) env fenv))
	       (meval-cond (cdr exp) env fenv))))))


;FONCTION ANNEXE DU MEVAL -> MEVAL-SETF
(defun msetf (place val env)
  (if (atom place)
      (let ((cell (assoc place env)))
	(if cell
	    (setf (cdr cell)
		  (meval val env))
	  (error "~s n'est pas une variable" place)))
    (cond ((eq (car place) 'car)
	   (setf (car (meval (cadr place) env))
		 (meval val env)))
	  ((eq (car place) 'cdr)
	   (setf (cdr (meval (cadr place) env))
		 (meval val env))))))

;FONCTION QUI FABRIQUE UNE FERMETURE
(defun make-closure (lmbd env)
  `(:closure ,lmbd . ,env))

;APPLIQUER UNE FERMETURE REVIENT UNE LAMBDA FONCTION
;DANS L'ENVIRONNEMENT
(defun meval-closure (clos args)
  (meval-lambda (cadr clos) args (cddr clos)))

(defun meval-trans-closure (clos args)
  (meval-trans-lambda (cadr clos) args (caddr clos)))

;FONCTION PERMETTANT D'ACCEDER A UNE LAMBDA FONCTION
;ASSOCIE AU SYMBOLE
(defmacro get-defun (symb) 
  `(get ,symb :defun))

(defun set-defun (symb lambda)
  (setf (get symb :defun) lambda ))

(defun get-defmacro (symb)
  (get symb :defmacro))

(defun set-defmacro (symb lambda)
  (setf(get symb :macro) lambda))

;QUI APPLIQUE UNE LAMBDA FONCTION QUELCONQUE
;A DES VALEURS D'ARG DANS UN CERTAINS ENVIRONNEMENT
(defun meval-lambda (lf le &optional env)
  (meval-body (cddr lf)
	      (make-env (cadr lf) le env)))

(defun meval-trans-lambda (exp arg &optional env)
  (expressionQuote (caddr exp) 
		   (make-env (+ 1 (length arg)) arg env)))

(defun expressionQuote (exp &optional env)
  (cond 
   ((atom exp) 
    nil)
   ((and (consp (car exp)) (eql (caar exp) ':var)) 
    (list 'aref env (cdar exp)) )
   ((and (consp (car exp))(eql (caar exp) ':set-var))
    (list 'aref env (cadar exp)))  
   ((saufP (car exp))   (expressionQuote (cdr exp) env))
   (t (cons  (car exp)(list* (expressionQuote (cdr exp) env) nil)))))

(defun saufP (exp)
  (if (or (eql `,exp ':vcall) (eql `,exp ':mcall)(eql `,exp ':lcall))
      exp
    nil))

;LISTE D'EXPRESSION EVALUABLE
;EVALUE CHAQUE EXPRESSION ET 
;RETOURNE LA LISTE DES VALEURS
(defun meval-body (le &optional env fenv)
  (if ( eq le nil)
      nil
    (if ( eq (cdr le) nil)
	(meval (car le) env fenv)
      (progn
	(meval (car le) env fenv)
	(meval-body (cdr le) env fenv)))))

;EVALUE CHAQUE EXPRESSION ET 
;RETOURNE LA LISTE DES VALEURS
(defun meval-args (le &optional env fenv)
  (if ( eq le nil)
      nil
    (if ( eq (cdr le) nil)
	(cons (meval (car le) env) nil)
      (cons
	(meval (car le) env)
	(meval-args (cdr le) env)))))

;EVALUE CHAQUE EXPRESSION ET 
;RETOURNE LA VALEUR DE LA PREMIERE
(defun meval-prog1 (le &optional env)
  (if ( eq le nil)
      nil
    (if ( eq (cdr le) nil)
	(meval (car le) env)
      (prog1
	(meval (car le) env)
	(meval-prog1 (cdr le) env)))))

;APPARIE LES PARAMETRES ET LES VALEURS
;ET RETOURNE LA LISTE D?ASSOCIATION L'ENV INCLUS
(defun make-env (lp lv &optional env)
  (if (null lp) 
      env
    (if (eql (car lp) '&rest)
	(let ((l (make-env-aux (cadr lp) lv)))
	  (if (eql (length lp) (+ 1 (length lv)))
	      (cons (cons (car l) (cdr l))env)
	    (cons (cons (car l) (cdr l)) env) 
	    ))
      (if (eql (car lp) '&optional)
	  (if (null (cddr lp))
	      (cons (cons (cadr lp) (car lv)) (make-env  (cddr lp) (cdr lv) env))
	    (cons (cons (cadr lp) (car lv)) (make-env (cons '&optional (cddr lp)) (cdr lv) env))
	    )
	(if (not lv)
	    env
	  (cons (cons (car lp) (car lv)) (make-env (cdr lp) (cdr lv) env)))))))

(defun make-env-aux (exp l)
  (if (null (car l))
      (list exp)
    (if (and (consp (car l)) (eql (length l) 1))
	(cons exp (car l))
      (cons exp l))))

;METS LE CONTENU DE LA PREMIERE CELLULE
;DANS LA SECONDE ET RETOURNE LA PREMIERE
(defun displace (cell newCell)
  (if (atom newCell)
      (setf newCell `(progn ,newCell)))
  (setf (car cell) (car newCell))
  (setf (cdr cell) (cdr newCell))
  cell)

;FONCTION DE META-CHARGEMENT
;QUI MEVAL UNE FONCTION SE TROUVANT DANS UN FICHIER
(defun mload (file)
  (labels ((lecture (o)
		    (let ((lu (read o nil nil nil)))
		      (if lu
			  (progn
			    (print lu)
			    (meval lu nil nil) 
			    (lecture o))
			't))))
    (lecture (open file))))

(let ((fe (list (list nil))))

  (defun init() (setf fe '((1))))
       
  (defun setenv (&optional fenv)
    (let ((cell (assoc 1 fe) ))
      (if cell
	  (progn
	    (setf (car cell) fenv)
	    (setf fe fenv)
	    fe))))
     
  (defun make-fenv (exp &optional env fenv)
    (progn (if (not fenv)
	       (init))
	   (if (atom exp)
	       nil
	     (displace (cons  (cons (caar exp)  (list ':closure  (cons 'lambda (cdar exp)) env fe)) 
			      (make-fenv (cdr exp) env fenv)))))))
	
	
(defun meval-labels (exp &optional env fenv)
  (let  ((ma (list (list (append  (caar fenv)(make-fenv (car exp) env  fenv) )))))
    (setenv (caar ma)) 
    (meval-progn (cdr exp) env (displace ma))))
              
