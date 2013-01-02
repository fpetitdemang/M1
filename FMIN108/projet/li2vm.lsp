(defun map-li2vm (lexpr env)
  (if (atom lexpr)
      ()
    (cons (li2vm (car lexpr) env) (map-li2vm (cdr lexpr) env)) 
    )
)

(defun li2vm (expr env)
  (cond
   ((atom expr) ())
   ((equal (car expr) :call) (li2vm-call expr env))
   ((equal (car expr) :if) (li2vm-if expr env))
   (t (print expr))
   )
  )

(defun li2vm-mcall (expr env)
  (let ((fun (get-defun(second expr))))
    ;taille environnement
    (print (cons ':stack (car expr))
    ;empile environnement
    (map-li2vm (second fun) env)
    ;empile instruction
    (map-li2vm (third fun) env)
    )
)

(defun li2vm-call (expr env)
  (map-li2vm (third expr) env)
   (print (cons (first expr) (second expr)))
)
(defun li2vm-if (expr env)
  (map-li2vm (second expr) env)
  (print (cons ':skipnil '2))
  (map-li2vm (third expr) env)
  (map-li2vm (fourth expr) env)
  )

;(li2vm '(:if (:call <= ((:var 1)(:const 1)))(:var 1)(:const 3)) ())