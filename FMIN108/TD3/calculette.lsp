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

(defun expr-pretty-print(expr pos marge)
 	if( (< expr-print-length(expr) marge);test si peu imprimer la ligne
 		prin1 expr
 	(let ((fun (car expr))(args (cdr expr)))
		(cond ((consp fun)
			if ((eq (car fun) lambda)
				(map-expr-pretty-print ( expr pos marge))
			(expr-pretty-print(expr (+ 2 pos) marge))
			);cas lambda fonction

		
		
		