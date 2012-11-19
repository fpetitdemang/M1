(setq po "#\(")
(setq pf "#\)")

(defun padding(n)
	 (loop for i from 1 to n do (write-char #\space )))

(defun expr-print-length(expr)
	(if (atom expr)
		(cond ((stringp expr);cas string
			(+ 2 length expr))
		((symbolp expr);cas symbole
			(length (symbol-name expr)))
		;rajouter quote
		(t; autre
			2))
	(+ 1 (expr-print-length (car expr))(expr-print-length (cdr expr))))) ;cas constante=cellule+2 pour compter les parentheses


(defun map-expr-pretty-print(lexpr pos marge)
	(if (atom lexpr)
		()
	(progn (expr-pretty-print (car lexpr) pos marge)
	()
	(map-expr-pretty-print (cdr lexpr) pos marge))))

;expr expression lisp
;pos position sur la ligne
;marge set display
(defun expr-pretty-print(expr pos marge)
	(padding pos);set position
 	(cond 
		((atom expr); si atome, imprime expr
			(prin1 expr))
		((< (+ pos ( expr-print-length expr)) marge );test si expr p ê imprimé
			(prin1 expr))
		;expr pt pas ê imprimé -> evaluation par cas
		((eq 'if (car expr));cas if
			(write-char #\( )
			(prin1 'if)
			(write-char #\space )
			(expr-pretty-print (second expr) (+ pos 0) marge)
			(terpri) 
			(expr-pretty-print (third expr) (+ pos 4) marge)
			(terpri)
			(expr-pretty-print (fourth expr) (+ pos 4) marge)
			(write-char #\) ))
		(t 
			(write-char #\()
			(prin1 (first expr ))
			(write-char #\space)
			(map-expr-pretty-print (cdr expr) (+ pos 1) marge) 
			(write-char #\))) 
	))

		
		
		