

(defun meta-eval (expr &optional lenv fenv) 
  (cond                      	;constantes
   ((and (atom expr) (constantp expr)) 
    expr)
   
					;variables
   ((symbolp expr) 
    (meta-eval-variable expr lenv fenv))

					;quote
   ((eq (car expr) 'quote) 
    (cadr expr))

	
					;lambda-expression
   ((and (consp (car expr)) (eq 'lambda (caar expr)))
    (meta-apply (car expr)
		(meta-eval-args (cdr expr) lenv fenv)
		lenv
		fenv))
		
					;cond
		((eq (car expr) 'cond)
		 (meta-eval-cond (cdr expr) lenv fenv))

					;let
		((eq (car expr) 'let) 
		 (meta-eval-body (cddr expr) (meta-make-env-let (cadr expr) lenv fenv) 
				 fenv ))
	
					;let*
		((eq (car expr) 'let*) 
		 (meta-eval-body (cddr expr) (meta-make-env-let* (cadr expr) lenv ) 
				 fenv ))

					;lambda,slambda			
	;	((and (symbolp (car expr))
		;      (eq (meta-closure-p (cdr (assoc (car expr) lenv))) 'lambda )
		 ;     (let ((lbd (cdr (assoc (car expr) lenv)))))
		 ;     (meta-apply (cdar lbd)
		;		  (meta-eval-args (cdr expr) lenv fenv)
		;		  (cdr lbd)
		;		  fenv)))
		
	;	((and (consp (car expr))
		;      (eq (caar expr) 'slambda))
	;	 (meta-apply (cdar expr)
		;	     (cons (cons (car expr) lenv)
		;		   (meta-eval-args (cdr expr) lenv))
		;	     lenv
		;	     fenv))
	;	((and (symbolp (car expr))
		     ;; (eq (meta-closure-p (cdr (assoc (car expr) lenv))) 'slambda )
		    ;  (let ((lbd (cdr (assoc (car expr) lenv)))))
		    ;  (meta-apply (cdar lbd)
		;		  (meta-eval-args (cdr expr) lenv fenv)
			;	  (cdr lbd)
			;	  fenv)))
	 
		((eq (car expr) 'apply)  ;apply
		 (let ((fct (meta-eval (cadr expr) lenv fenv)))
		   (if (and (consp fct)
			    (consp (car fct))
			    (eq 'lambda (caar fct)))
		       (meta-eval-body (cddar fct)
				      (bind-keys (cadar fct) (meta-eval-args* (cddr expr) lenv fenv) (cdr fct))
				      fenv)
		     (apply fct (meta-eval-args* (cddr expr) lenv fenv)))))

	
					;setf
		((eq 'setf (car expr))
		 (meta-setf (cdr expr) lenv fenv))
					;defun
		((eq (car expr) 'defun) 
		 (progn 	(setf (get (cadr expr) :defun) (cons (cons 'lambda (cddr expr)) 
								     lenv))
				(cadr expr)
           
				 ))
		
					;defmacro
		((eq (car expr) 'defmacro) 
		 (progn 	(setf (get (cadr expr) :defmacro) `(lambda ,@(cddr expr)))
				(cadr expr)))

					;appel fonction
		((get (car expr) :defun) 
		 (let 	((fct (get (car expr) :defun)))
		   (meta-apply (car fct)
			     ;  (meta-eval-args (cdr expr)  lenv fenv) lenv fenv)))
	                          (meta-eval-args (cdr expr)  lenv  fenv) (cdr fct)  fenv)))
					;if
		((eq 'if (car expr))
		 (if (meta-eval (cadr expr) lenv fenv)
		     (meta-eval (car (cddr expr)) lenv fenv)
		   (meta-eval (cadr (cddr expr)) lenv fenv)))
                                       
					;and
		((eq (car expr) 'and)
		 (meta-eval-and (cdr expr) lenv fenv))
		
		                       ;progn
		((eq 'progn (car expr))
		 (meta-progn (cdr expr) lenv fenv))
      

					;prog1
		((eq 'prog1 (car expr))
		 (let ((first (meta-eval (cadr expr) lenv fenv)))
		   (meta-progn (cddr expr) lenv fenv)
		   first))
					;while
		((eq 'while (car expr))
		 (if (meta-eval (cadr expr) lenv fenv)
		     (progn (meta-progn (cddr expr) lenv fenv)
			    (meta-eval expr lenv fenv))
		   nil))


					;appel macro
		((get (car expr) :defmacro) 
		 (meta-eval-appel-macro expr lenv fenv))

					;function
		((eq (car expr) 'function)
		 (if (symbolp (cadr expr)) 
		     (or (get (cadr expr) :defun) (symbol-function (cadr expr)))
		   `(,(cadr expr) . ,lenv)))

					;flet
		((eq 'flet (car expr))
		 (meta-eval-body (cddr expr) lenv (make-make-fenv (cadr expr) lenv fenv)))
                              
					;labels
		((eq 'labels (car expr))
		 (let ((fenv (meta-make-fenv (cadr expr) lenv fenv)))
		   (meta-progn (cddr expr)
			       (nconc (meta-change-env fenv fenv) lenv) fenv)))
					;fenv
		((assoc (car expr) fenv)
		 (let ((clos (cdr (assoc (car expr) fenv))))
		   (meta-apply (car clos)
			       (meta-eval-args (cdr expr)  lenv fenv)
			       lenv
			       fenv)))
		
	
	


					;macro-function
		((macro-function (car expr))
		 (meta-eval (displace expr (macroexpand expr)) lenv fenv))
		
					;symbol-function
		((fboundp (car expr))  
		 (apply (symbol-function (car expr)) (meta-eval-args (cdr expr) lenv fenv)))


					;load
		((eq (car expr) 'load)
		 (mload (cadr expr)))

		(t (print "je ne sais pas evaluer ~s" expr))))



;##############################################################################################################################
(defun meta-eval-variable (expr lenv fenv)
  (let 	((cell (assoc expr lenv)))
    (if 	cell
	(cdr cell)
      (error "variable inconnue ~S" expr))))

(defun displace (expr nexpr)
  (if 	(atom nexpr)
      (setf expr nexpr)
    (progn 	(setf (car expr) (car nexpr))
		(setf (cdr expr) (cdr nexpr))
		expr)))


(defun meta-eval-appel-macro (expr lenv fenv)
  (let* 	(	(lmbd (get (car expr) :defmacro))
			(nenv (bind-keys (cadr lmbd) (cdr expr) lenv))
			(nexpr (meta-eval-body 	(cddr lmbd)
						nenv
						fenv
						)))
    (meta-eval (displace expr nexpr) nenv fenv)))


(defun meta-eval-args (args lenv fenv)
  (if 	(consp args)
      (cons 	(meta-eval (car args) lenv fenv)
		(meta-eval-args (cdr args) lenv fenv ))))

(defun meta-eval-args* (args env fenv)
  (if 	(consp args)
      (if (null (cdr args))
	  (meta-eval (car args) env fenv)
	(cons 	(meta-eval (car args) env fenv)
		(meta-eval-args* (cdr args) env fenv)))))

;(defun meta-make-env1 (fns)
 ;  (if fns
   ;    (list (caar fns) (cons 'lambda  (cons (cadar fns) (cddar fns))))))

(defun meta-make-fenv (lfn env fenv)
  (if (null lfn)
      fenv
    (cons `(,(caar lfn) (lambda ,@(cdar lfn)) ,env ,@fenv)
	  (meta-make-fenv (cdr lfn) env fenv))))

(defun meta-change-env (env fenv)
     (if (null env)
         fenv
      ; (progn  (setf (cdr (cddar env)) fenv)
        (progn  (setf (car (cddar env)) fenv)	       
	       (meta-change-env (cdr env) fenv))))

(defun meta-eval-and (expr lenv fenv)
  (if expr
      (if (cdr expr)
          (and (meta-eval (car expr) lenv fenv) (meta-eval-and (cdr expr) lenv fenv))
	(meta-eval (car expr) lenv fenv))
    t))

(defun meta-closure-p (x)
  (if (consp x)
      (meta-lambda-p (car x))))


(defun meta-lambda-p (x)
   (if (and (consp x)
         (member (car x) '(lambda slambda)))
       (car x)))


(defun meta-apply (lbd args lenv fenv)
    (meta-progn (cddr lbd)
              (bind-keys (cadr lbd) args lenv) fenv))

(defun meta-progn (exprs lenv fenv)
    (cond ((atom exprs) ())
          
   ; (cond ((atom exprs) lenv)
	  ((cdr exprs)
	    (meta-eval (car exprs) lenv fenv)
            (meta-progn (cdr exprs) lenv fenv))
         (t (meta-eval (car exprs) lenv fenv))))

(defun binds (params args lenv)
        (nconc (mapcar 'cons params args) lenv))


(defun meta-eval-body (expr lenv fenv) 
  (meta-progn expr lenv fenv))
          

(defun meta-make-env-let (varvals env fenv)
  (if (null varvals)
      env
    (let 	((val (meta-eval (cadar varvals) env fenv)) (var (caar varvals)))
      (cons (cons var val) (meta-make-env-let (cdr varvals) env fenv )))))

(defun make-let-env (let-list lenv fenv)
  (if (null let-list)
      ()
    (cons (cons (caar let-list)
		(meta-eval (cadr (car let-list)) lenv fenv))
		(make-let-env (cdr let-list) lenv fenv))))



(defun meta-make-env-let* (varvals lenv fenv)
   (if 	(null varvals)
      lenv
     (let 	((val (meta-eval (cadar varvals) lenv fenv)) (var (caar varvals)))
       (meta-make-env-let* (cdr varvals) (cons (cons var val) lenv) fenv))))

(defun meta-eval-cond (expr lenv fenv)
  (if 	(eq (caar expr) 't)
      (meta-eval (cadar expr) lenv fenv)
    (if 	(meta-eval (caar expr) lenv fenv)
	(meta-eval (cadar expr) lenv fenv)
      (meta-eval-cond (cdr expr) lenv fenv))))

(defun bind-keys(params args lenv)
   (cond ((null params)
	  (if args 
	      (error  "trop d'arguments" args)
	    lenv))
	 ((eq (car params) '&optional)
          (bind-opt (cdr params) args lenv))
         ((eq (car params) '&rest)
          (cons (cons (cadr params)  args )lenv))
        
        ((null args)
       ;  (error  "trop peu d'arguments" params)

          lenv                 )
         

         (t (bind-keys (cdr params) (cdr args)
                       (cons (cons (car params) (car args)) lenv)))))

(defun bind-opt (params args lenv)
  (cond ((null params) lenv)
	((eq (car params) '&rest)
	 (cons (cons (cadr params) args) lenv))
        ((null args)
         (bind-def  params lenv))
         
         (t (bind-opt (cdr params) (cdr args)
                       (cons (cons (if (consp (car params))
                                       (caar params)
                                       (car params))
                                     (car args))
                              lenv)))))


(defun bind-def (params lenv)
  (cond ((null params) lenv)
	((eq (car params) '&rest)
	 (cons (cons (cadr params) nil) lenv))
	(t (let* ((tete (car params))
		  (all   (cons (cons (car tete) (cadr tete)) lenv)))
	     (bind-def (cdr params) all)))))
                
	 


(defun meta-setf (expr lenv fenv)
  (if (null expr)
      lenv
    (progn (meta-setf-case (car expr) (meta-eval (cadr expr) lenv fenv) lenv fenv)
	   (meta-setf (cddr expr) lenv fenv))))

(defun meta-setf-case(expr val lenv fenv)
  (cond ((symbolp expr)
	 (let ((binding (assoc expr lenv)))
	   (if  binding
	       (setf (cdr binding) val)
	     (progn  (setf lenv (cons (cons expr val) lenv)) (print lenv)))))
	((eq 'car (car expr))
	 (setf (caadr (cdr (assoc (cadr expr) lenv))) val))
	((eq 'cdr (car expr))
	 (setf (cdadr (cdr (assoc (cadr expr) lenv))) val))
	((eq 'get (car expr))
	 ())))
	

	     

(defun mload (nom)
  (print "loading")
  (let ((stream (open nom :direction :input
		     :if-does-not-exist :error
		     )))
     (loop until (eq :eof (setf m (read stream nil :eof)))
       do   (meta-eval m nil nil))
           (close stream))
	   ( print "le fichier est charge") nom)







;**************************************************************************************



(defun leng-c (l c)
  (if (atom l)
      (apply c 0 ()  )
      (leng-c (cdr l)
               #'(lambda(x)(apply c (1+ x) () )))))

(defun copylist(l)
  (if (atom l)
      l
  (cons (car l) (copylist(cdr l)))))

(defun copylist-c (l c)
  (if (atom l)
      (apply c l ())
      (copylist-c (cdr l) #'(lambda(x)(apply c (cons (car l)  x) ())))))

(defun copytree (tree)
  (if (atom tree)
        tree
    (cons (copytree (car tree))
	  (copytree (cdr tree)))))

(defun copytree-c (tree c)
  (if (atom tree)
      (apply c tree ())
      (copytree-c (car tree) #'(lambda(x)(apply c (cons x (copytree-c (cdr tree))))()))))


