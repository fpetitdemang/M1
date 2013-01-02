(defun make-env(ll lv env)
 (if (atom ll)
    (if (atom lv)
	()
	(error "make-env : pb de taille pour les variables :~s ou pour les valeurs :~s" ll lv))
 (if (atom lv)
     (error "make-env  : un autre pb de taille pour les variables :~s ou pour les valeurs :~s" ll lv)
    (cons(cons(car ll)(car lv))(make-env (cdr ll)(cdr lv)env)))))

(defun displace(old new)
  (if (atom new)
      (setf new (progn new))
    ())
  (setf (car old) (car new))
  (setf (cdr old) (cdr new))
 old)

(defun meval-body(l env)
  (if (atom l)
      (error "meval-body : probleme de parametre"))
  (if (atom (cdr l))
      (meval (car l) env)
    (progn (meval (car l) env)
	   (meval-body (cdr l) env))))

(defun meval-args(l env)
  (if (atom l)
      ()
    (cons (meval (car l) env)(meval-args (cdr l) env))))

(defun meval-lambda(ll lv env)
  (meval-body (cddr ll)
	      (make-env (cadr ll)
			lv
			env)))

(defun get-defun (ll)
  (get ll :defun))

(defun set-defun (ll body)
  (setf (get ll :defun) body))

(defun get-defmacro (ll)
  (get ll :defmacro))

(defun set-defmacro (ll body)
  (setf (get ll :defmacro) body))

(defun make-aux-let(expr env)
    (if (null expr)
	env
      (if (atom(car expr))
	  (cons (cons (car expr) ())(make-aux-let(cdr expr) env))
	(cons (cons (caar expr) (meval (cadar expr) env)) (make-aux-let (cdr expr) env)))))

(defun mload (file)
  (let ((stream (open file :direction :input)) (EOF (gensym)))
    (let ((r (read stream () EOF)))
      (loop until (eql r EOF) do (progn (meval r ())
					(setf r (read stream () EOF)))))))

(defun meval(expr env)
  (cond 

   ;constante
   ((and (atom expr)(constantp expr)) 
    expr)
	 
    ;variable
   ((symbolp expr)
    (let ((cell(assoc expr env)))
      (if cell
	  (cdr cell)
	(error "meval : variable ~S inconnue" expr))))

   ;lambda exp
   ((and (consp (car expr))(eq 'lambda(caar expr)))
    (meval-lambda (car expr)
		  (meval-args (cdr expr) env)
		  env))

   ;lambda exp sans arg
   ((eq 'lambda (car expr))
    (meval-lambda  expr
		   (meval-args (cadr expr) env)
		   env))
   
   ((not (symbolp (car expr)))
    (error "meval : ~S n'est pas un symbole." (car expr)))
   
   ((eq (car expr) 'quote)
    (cadr expr))
   
   ((eq (car expr) 'if)
    (if (meval (cadr expr) env)
	(meval (caddr expr) env)
      (meval (cadddr expr) env)))
   
   ((eq (car expr) 'let)	 
    (meval-body(cddr expr) (make-aux-let (cadr expr) env)))
   
   ((or (eq (car expr) 'setf)  (eq (car expr) 'setq))
    (if (symbolp (cadr expr))
	(let ((cell (assoc (cadr expr) env)))
	  (if cell
	      (setf (cdr cell)
		    (meval (caddr expr) env))
	    (error "meval ~s ~s n'est pas dans l'environnement ~s " (car expr) (cadr expr) env )))
      ;else 
      
      (cond  ;((fboundp (caadr expr))
	     ; (eval `(setf ( ,(caadr expr) ,@(meval (cadadr expr) env) )
	;		    ',(meval (caddr expr) env)   ))  )
            ((eql (caadr expr) 'cdr)
	     (setf (cdr (meval (cadadr expr) env))
		   (meval (caddr expr) env)))
	    ((eql (caadr expr) 'car)
	     (setf (car (meval (cadadr expr) env))
		   (meval (caddr expr) env)))
	    ((eql (caadr expr) 'get)
	     (setf (get (meval (cadadr expr) env) (meval (car (cddadr expr)) env ))
		   (meval (caddr expr) env)))

	    (t (error "setf : NYI ~s " expr ))
)
;      (error "meval setf NYI fonctions  ~s " expr)

      ))

;   ((eq (car expr) 'setq)
 ;   (meval (cons 'setf (cdr expr)) env))


   ; ((eq (car expr) 'setq)
   ; (setf (cdr (assoc (cadr expr) env))
	;  (meval (caddr expr)env)))
    

   
   ((eq (car expr) 'defun)
    (set-defun (cadr expr) `(lambda ,@(cddr expr))))
   
   ((eq (car expr) 'defmacro)
    (set-defmacro (cadr expr) `(lambda ,@(cddr expr))))
   
   ((eq (car expr) 'progn)
    (meval-body (cdr expr) env))
   
   ;fonction meta-definie
   ((get-defun (car expr))
    (meval-lambda (get-defun (car expr))
		  (meval-args (cdr expr) env)
		  env))
   
   ;macro meta-definie
   ((get-defmacro (car expr))
    (meval (displace expr (meval-lambda (get-defmacro (car expr))
					(cdr expr)
					()))
	   env))
   
   ;macro predefinie
   ((macro-function (car expr))
    (meval (displace expr (macroexpand-1 expr)) env))
   
   ((special-form-p(car expr))
    (error "meval : ~S NYI" expr))
   
   ((not(fboundp(car expr)))
    (error "meval : La fonction ~S n'est pas definie" (car expr)))
   
   ;fonction predefinie
   (t(apply (symbol-function (car expr))
	    (meval-args (cdr expr) env)))))
