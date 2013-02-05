(defun mtrans (exp &optional env fenv)
  (cond
   ((and (atom exp)
	 (constantp exp)) 
    (cons ':const exp))
   
   ((and (atom exp)
	 (eql '&rest exp))
    `:rest)

   ((atom exp) 
    (let ((cell (m-assocpval exp (reset) env)))
      (if cell
	  (if (> (car cell) 0) 
	      (progn
		`(:cvar ,(car cell) . ,(cadr cell)))
	    (if (= (car  cell) 0) 
		`(:var . ,(cadr cell))))
	(error "variable inconnue"))))
   
   ((eql (car exp) 'system::backquote)
    exp) 
          
   ((and (consp exp)
	 (atom (car exp))
	 (constantp (car exp)))
    (list* ':const exp))
   
   ((and (consp (car exp)) 
	 (eql (caar exp) 'lambda))  
    `(:lambda-apply ,(mtrans (car exp) env fenv)
		    ,(mtrans-args (cdr exp) env fenv)))

   ((eql 'lambda (car exp))
    (let ((env2 (cons env (make-env-trans (cadr exp) nil (reset) nil))))
      ` (:lambda ,(mtrans-args (car(cdr exp)) env2) ,(mtrans (caddr exp)  env2  fenv))))
   
   ((eql (car exp) 'defun)
    (let ((env2 (make-env-trans (caddr exp) nil (reset) nil)))
      (if (null env2) (setf env env) (setf env (cons env env2)))
      (setf (get (cadr exp) :unknown) `(:closure ,(cadr exp)) )
      `(:defun  ,(cadr exp) (:lambda ,(mtrans-args (car (cddr exp))  env) ,(mtrans (cadddr exp)  env   fenv)) (,@env)  (,@fenv))))
       
   ((eql (car exp) 'defmacro)
    (let ((env2 (make-env-trans (caddr exp) nil (reset) nil)))
      (if (null env2) (setf env env) (setf env (cons env env2)))
      (setf (get (cadr exp) :unknown) `(:closure ,(cadr exp)) )
      `(:defmacro  ,(cadr exp) (:lambda ,(mtrans-args (car (cddr exp))  env) ,(mtrans (cadddr exp)  env   fenv)) (,@env)  (,@fenv))))
   
   ((eql 'quote (car exp))
    `(:quote ,(cadr exp)))

   ((eq 'setf (car exp))
    (if (atom (cadr exp)) 
	(let ((cell (m-assocpval (cadr exp) (reset) env)))
	  (if (null cell) (error "variable inconnue!!")
	    (if (eql 0 (car cell))
         	`(:set-var ,(cadr cell) . ,(mtrans (caddr exp) env fenv))
	      `(:set-cvar ,(car cell) ,(cadr cell) . ,(mtrans (caddr exp) env fenv)))))
      (let ((expr (mtrans (cadr exp) env fenv)))
	`(:set-var ,expr . ,(mtrans (caddr exp) env fenv)))))
                  
   ((eql 'progn (car exp))
    (cons ':progn (mtrans-progn (cdr exp) env fenv)))
   
   ((eql 'if (car exp))
    `(:if ,(mtrans (cadr exp) env fenv) ,(mtrans (caddr exp) env fenv) ,(mtrans (cadddr exp) env fenv)))
   
   ((eql 'cond (car exp))
    `(:cond ,(cdr exp) ,env ,fenv))
   
   ((eql (car exp) 'apply)
    (progn
      (let ((rep (mtrans  (cadr exp) env fenv) ))
	(if (or 
	     (eql (car rep) ':var) 
	     (eql (car rep) ':cvar))
	    (list* ':lambda-apply rep  (mtrans-args (apply #'list*   (dequote (cddr exp))) env fenv))
	  (if (eql (length (cddr exp)) 1) 
	      (if (eql (car rep) ':lambda)
		  (list* ':lambda-apply rep  
			 (mtrans-args 
			  (apply #'list*   
				 (list (dequote (caddr exp)))) env fenv))
		(append rep 
			(mtrans-args  
			 (apply #'list* 
				(list (dequote (caddr exp)))) env fenv))) 
	    (if (eql (car rep) ':lambda)
                (list* ':lambda-apply rep  
		       (mtrans-args 
			(apply #'list* (dequote  (cddr exp))) env fenv))
	      (progn
                (list* ':apply  rep 
		       (mtrans-args  
			(apply #'list* (dequote   (cddr exp))) env fenv))))))))) 

   ((eq (car exp) 'function)
    (list ':function (mtrans-function (cadr exp) env fenv) ))
       
   ((or 
     (eql 'let (car exp))
     (eql 'let* (car exp) ))
    (let ((env2 (set-var* (reset) (cadr exp) env)))
      (setf env (cons env (p-pointe (cadr exp))))
      `(:let ,env2 ,(mtrans (cons 'progn (cddr exp)) env fenv))))
      
   ((eql 'labels (car exp))
    (cons ':labels (mtrans-labels (cdr exp) env fenv)))
   
   ((and 
     (atom (car exp))
     (get (car exp) :defun))
    (let ((def (get (car exp) :defun)))
      (list* ':mcall (car exp)  
	     (mtrans-args (cdr exp) env (cons (cadddr def) (cdr fenv))))))
          
   ((and 
     (consp (car exp)) 
     (eql 'lambda (caar exp)))
    `(:lambda ,(car exp) ,(mtrans (cdr exp) env env)) )
   
   ((mtrans-local-function (car exp) env (caar fenv))
    (list* ':lcall (car exp)
	   (mtrans-args (cdr exp) env fenv)))
   
   ((and 
     (atom (car exp))
     (get (car exp) :defmacro))
    (let ((def (get (car exp) :defmacro)))
      (list* ':macrocall (car exp)  
	     (mtrans-args (cdr exp) env (cons (cadddr def) (cdr fenv))))))
     
   ((and 
     (consp exp) 
     (or 
      (special-form-p (car exp))
      (special-operator-p (car exp))))
    `(:special  ,exp ,env ,fenv))
   
   ((eql 'time (car exp)) 
    `(:time ,(mtrans (cadr exp) env fenv)))
     
   ((eql 'load (car exp))
    `(:mload ,(cadr exp) ,env ,fenv))
   
   ((get (car exp) :unknownmacro)
    (list* ':macrocall (car exp) (mtrans-args (cdr exp) env fenv)))
   
   ((get (car exp) :unknown)
    (list* ':mcall (car exp) (mtrans-args (cdr exp) env fenv)))
   
   ((symbol-function (car exp))
    (list* ':vcall (car exp) (mtrans-args (cdr exp) env fenv) ))
   
   (t
    (error "execution en CLISP de ~s"(car exp)))))       
       

(defun p-pointe (l &optional env fenv)
  (if (atom l)
      env
    (if (and (consp (car l)) (eql (length (car l)) 2))
	(cons (cons (caar l) (mtrans (cadar l) env fenv)) (p-pointe (cdr l) env fenv))
      nil)))

(defun make-env-trans (lp lv a1 &optional env)
  (cond ((null lp)
	 (progn a1 env))
	((eql (car lp) '&rest)
	 (let ((l (cons (cadr lp) lv)))
	   (if (eql (length lp) (+ 1 (length lv)))
	       (cons (cons (car l) (cdr l))env)
	     (cons (cons (car l) (cdr l)) env))))
	((eql (car lp) '&optional)
	 (cons (cons (cadr lp) (car lv)) (make-env-trans (cddr lp) (cdr lv) (compteur++) env)))
	('t (cons (cons (car lp) (car lv)) (make-env-trans (cdr lp) (cdr lv) (compteur++) env)))))

(defun m-assocpval (var b &optional env fenv)
  (if (null env)
      nil
    (let ((app (apparait var (cdr env)) ))
      (if app
	  (list b (- (length env) (length app)))
	(m-assocpval var (compteur++) (car env) fenv)))))


(defun apparait(var liste-couple)
  (if (null liste-couple)
      nil
    (if (eql var (caar liste-couple))
	liste-couple
      (apparait var (cdr liste-couple)))))


(defun mtrans-args (exp &optional env fenv)
  (if (atom exp) nil
    (cons  (mtrans (car exp) env fenv)  
	   (mtrans-args (cdr exp) env fenv))))


(defun mtrans-progn (exp &optional env fenv)
  (if (atom exp)
      nil
    (let ((rep (mtrans (car exp) env fenv) ))
      (cons rep (mtrans-progn (cdr exp) env fenv)))))

(defun mtrans-function (exp &optional env fenv)
  (if (and (consp exp) (eql 'lambda (car exp)))
      (mtrans exp env fenv)
    (if (get exp :defun)
	`(:mcall ,exp)
      `(:vcall ,exp))))

(defun mtrans-local-function (nom &optional env fenv)
  (if (null fenv)
      nil
    (if (and (consp (car fenv)) (eql nom (caar fenv)))
        (cdr (car fenv))
      (mtrans-local-function nom env (cdr fenv)))))

(defun explabels-bis (exp &optional env fenv)
  (if (null  exp)
      nil
    (cons (list* ':setf-fun (caar exp)  (mtrans (cons 'lambda (cdar exp)) env fenv) nil) 
	  (expLabels-bis (cdr exp) env fenv))))

(defun explabels (exp env fenv)
  (list* (explabels-bis (car exp) env fenv) (mtrans-progn (cdr exp) env fenv)))

(defun mtrans-labels (exp &optional env fenv)
  (let  ((ma (list (list (append  (caar fenv)(make-fenv-trans (car exp) env  fenv) )))))
    (setenv-trans (caar ma)) 
    (explabels exp env (displace-trans ma))))


(defun dequote (exp)
  (cond 
   ((atom exp)
    exp)
   ((eql 'quote (car exp)) 
    (cadr exp)) 
   ((and 
     (consp (car exp))
     (eql (caar exp) 'quote))
    (cons (cadr (car exp)) (dequote (cdr exp))))
   (t
    (cons (car exp) (dequote (cdr exp))))))

(defun displace-trans (exp2)
  (let ((exp1 (cons (car exp2) (cdr exp2)) ))
    exp1))

(let ((fe (list (list nil))))

  (defun initialise() (setf fe '((1))))
         
  (defun setenv-trans (&optional fenv)
    (let ((cell (assoc 1 fe) ))
      (if cell
	  (progn
	    (setf (car cell) fenv)
	    (setf fe fenv)
	    fe ))))
  
  (defun make-fenv-trans (exp &optional env fenv)
    (progn 
      (if (not fenv)
	  (initialise))
      (if (atom exp)
	  nil
	(displace-trans (cons  (cons (caar exp) (list ':closure (cons 'lambda (cdar exp)) env fe)) (make-fenv-trans (cdr exp) env fenv)))))))


(let ((etiq 0)) 

  (defun compteur () etiq) 
  
  (defun compteur++ () (setf etiq (+ etiq 1)))
  
  (defun reset () (setf etiq 0)))


