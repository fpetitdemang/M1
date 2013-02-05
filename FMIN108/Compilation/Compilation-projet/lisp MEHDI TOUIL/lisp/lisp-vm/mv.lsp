;mise a jour de la table de hash (pour rendre plus clair le code)
(defun setHash (key val table)
	(setf (gethash  key table ) val  )
) 

;mise a jour de la memoire : modification de la memoire a l enplacement adresse
(defun setMem (mv adresse valeur)
	(setf (aref (get mv :memMV) adresse) valeur)
)

;Creation de la VM (utilisation de mv pour ne pas interferer avec 'vm dans un debug)
(defun mvMAKE (mv mem)
	(setf (get mv :memMV) (make-array mem))
	(setf (get mv :tailleMem ) mem)
	(setf (get mv :R0 ) 0)
	(setf (get mv :R1 ) 0)
	(setf (get mv :R2 ) 0)
	(setf (get mv :SP ) 0)
	(setf (get mv :BP ) 100)
	(setf (get mv :PC ) 0)
	(setf (get mv :FP ) 0)
	(setf (get mv :DE ) 0)
	(setf (get mv :DPP ) 0)
	(setf (get mv :DPG ) 0)
	(setf (get mv :LC ) 0)
	(setf (get mv :etiqGlob ) (make-hash-table :size mem))
	(setf (get mv :etiqGlobNR ) (make-hash-table :size mem))
)


;fonction d'initialisation de la MV
(defun mvINIT (mv)
	(setf (get mv :memMV) (make-array (get mv :tailleMem)))
	(setf (get mv :PC ) (- (get mv :tailleMem) 1 ))
	(setf (get mv :FP ) 0)
	(setf (get mv :R0 ) 0)
	(setf (get mv :R1 ) 0)
	(setf (get mv :R2 ) 0)
	(setf (get mv :BP ) 100)
	(setf (get mv :SP ) (get mv :BP))
	(setf (get mv :DE ) 0)
	(setf (get mv :DPP ) 0)
	(setf (get mv :DPG ) 0)
	(setf (get mv :LC ) (- (get mv :tailleMem) 1))
	(setf (get mv :etiqGlob ) (make-hash-table :size (get mv :tailleMem)))
	(setf (get mv :etiqGlobNR ) (make-hash-table :size (get mv :tailleMem)))
	(setf (gethash 'nb (get mv :etiqGlobNR )) 0)

)


;fonction pour les adressages "sources"
(defun src (mv expr)
	(cond
		((null expr) 0) ;null
		((numberp expr) (aref (get mv :memMV) expr)) ;mode direct (ex : 100)
		((eql (car expr) ':DIESE) (cadr expr)) ;les constantes (ex : :DIESE 5)
		((eql (car expr) ':Rx) (get mv (cadr expr))) ;les registres (ex : :Rx :R0)
		((numberp (car expr)) (aref (get mv :memMV) (+ (car expr) (src mv (cadr expr))))) ;mode indexe (ex : 4 (:Rx :R0))
		((eql (car expr) ':*) (aref (get mv :memMV) (src mv (cadr expr)))) ;mode indirect
		((eql (car expr) 'LOC) ;variables ou parametres
			(let	
				(
					(nextFP (get mv :FP))
				)
				(if (eql (caddr expr) ()) (setf (caddr expr) 0))
				(loop while (> (aref (get mv :memMV) (+ 3 nextFP)) (caddr expr))
					do (setf nextFP (aref (get mv :memMV) (+ 2 nextFP )))
				)
				(if (< (cadr expr) 0 )
					(aref (get mv :memMV) (- nextFP  (aref (get mv :memMV) nextFP ) 1 (cadr expr)))
					(aref (get mv :memMV)  (+  4 nextFP  (cadr expr)))
				)
			)
		)
		(t
			(car expr) ;par exemple (1 2 3)
		)
	)
)

;fonction pour les adressages "destinations"
(defun dst (mv expr)
	(cond
		((null expr) 0) ;null
		((numberp expr) expr) ;mode direct (ex : 100)
		((eql (car expr) ':Rx) (cadr expr)) ;les registres (ex : :Rx :R0)
		((numberp (car expr)) (+ (car expr) (src mv (cadr expr)))) ;mode indexe (ex : 4 (:Rx :R0))
		((eql (car expr) ':*) (src mv (cadr expr))) ;mode indirect
		((eql (car expr) '@) (if (symbol-function (cadr expr)) (cadr expr)))
		((eql (car expr) 'LOC)
			(let	(
					(nextFP (get mv :FP))
				)
				(loop while (> (aref (get mv :memMV) (+ 3 nextFP)) (caddr expr))
					do (setf nextFP (aref (get mv :memMV) (+ 2 nextFP )))
				) 
				(if (< (cadr expr) 0 )
					(- nextFP (aref (get mv :memMV) nextFP) 1 (cadr expr))  
					(+  4 nextFP  (cadr expr))
				)
			)
		)
	)
)

;MOVE
(defun mvMOVE (mv source dest)
	(if (numberp dest) (setMem mv dest source)) ;si la destination est une adresse memoire
	(setf (get mv dest) source) ;les autres cas (ex: les registres)
)

;ADD
(defun mvADD (mv expr1 expr2)
	(mvMOVE mv (+ expr1 (get mv expr2)) expr2) ;MOVE expr2 (+ expr1 expr2)
)

;SUB
(defun mvSUB (mv expr1 expr2)
	(mvMOVE mv (- (get mv expr2) expr1) expr2) ;MOVE expr2 (- expr2 expr1)
)

;MULT
(defun mvMULT (mv expr1 expr2)
	(mvMOVE mv (* expr1 (get mv expr2)) expr2) ;MOVE expr2 (* expr1 expr2)
)

;DIV
(defun mvDIV (mv expr1 expr2)
	(mvMOVE mv (/ (get mv expr2) expr1) expr2) ;MOVE expr2 (/ expr2 expr1)
)

;PUSH
(defun mvPUSH (mv expr)
	(progn 
		(mvINCR mv :SP) ;on incremente le stack pointer
		(mvMOVE mv expr (dst mv '(:* (:Rx :SP)))) ;MOVE expr *SP
	)
)

;POP
(defun mvPOP (mv expr)
	(prog1
		(mvMOVE mv (src mv '(:* (:Rx :SP))) expr) ;MOVE *SP expr
		(mvDECR mv :SP) ;on decremente le stack pointer
	)
)

;JMP
(defun mvJMP (mv expr)
	(mvMOVE mv expr (dst mv '(:Rx :PC))) ;MOVE expr PC
)

;JSR
(defun mvJSR (mv expr)
	(progn
		(mvPUSH mv (src mv '(:Rx :PC)))
		(cond
			((numberp expr) (mvJMP mv (dst mv expr)))
			((symbol-function expr) (mvApply mv expr))
		)
	)
)

;CMP
(defun mvCMP (mv expr1 expr2)
	(cond 
		(
			(and (numberp expr1) (numberp expr2))
			(if (= expr1 expr2)
				(progn
					(setf (get mv :DPP) 0)
					(setf (get mv :DE) 1)
					(setf (get mv :DPG) 0)
				)
			)
			(if (< expr1 expr2)
				(progn
					(setf (get mv :DPP) 1)
					(setf (get mv :DE) 0)
					(setf (get mv :DPG) 0)
				)
			)
			(if (> expr1 expr2)
				(progn
					(setf (get mv :DPP) 0)
					(setf (get mv :DE) 0)
					(setf (get mv :DPG) 1)
				)
			)
		)
		(t
			(if (eql expr1 expr2)
				(progn 
					(setf (get mv :DPP) 0)
					(setf (get mv :DE) 1)
					(setf (get mv :DPG) 0)
				)
				(progn 
					(setf (get mv :DPP) 0)
					(setf (get mv :DE) 0)
					(setf (get mv :DPG) 0)
				)
			)
		)
	)
)

(defun mvJEQ (mv expr)
	(if (= (get mv :DE) 1) (setf (get mv :PC) expr))
)

(defun mvJL (mv expr)
	(if (= (get mv :DPP) 1) (setf (get mv :PC) expr))
)

(defun mvJG (mv expr)
	(if (= (get mv :DPG) 1) (setf (get mv :PC) expr))
)

(defun mvJLE (mv expr)
	(if (= (get mv :DPG) 0) (setf (get mv :PC) expr))
)

(defun mvJGE (mv expr)
	(if (= (get mv :DPP) 0) (setf (get mv :PC) expr))
)

(defun mvJNE (mv expr)
	(if (= (get mv :DE) 0) (setf (get mv :PC) expr))
)

;RTN
(defun mvRTN (mv)
	(progn
		(mvMOVE mv (src mv '( 1 (:Rx :FP)) ) (dst mv '(:Rx :SP)))
		(mvMOVE mv (src mv '( 4 (:Rx :FP)) )  (dst mv '(:Rx :PC)))
		(mvMOVE mv (src mv '( 2 (:Rx :FP)) )  (dst mv '(:Rx :FP)))
	)
)

;Incrementation d'une expression
(defun mvINCR (mv expr)
	(setf (get mv expr) (+ (get mv expr) 1))
)

;Decrementation d'une expression
(defun mvDECR (mv expr)
	(setf (get mv expr) (- (get mv expr) 1))
)

;Execution des commandes assembleur dans le langage de la MV
(defun mvEXEC (mv expr)
	(cond 
		((null expr) ())
		((eql (car expr) 'MOVE) (mvMOVE mv (src mv (cadr expr)) (dst mv (caddr expr))))
		((eql (car expr) 'ADD) (mvADD mv (src mv (cadr expr)) (dst mv (caddr expr))))
		((eql (car expr) 'SUB) (mvSUB mv (src mv (cadr expr)) (dst mv (caddr expr))))
		((eql (car expr) 'MULT) (mvMULT mv (src mv (cadr expr)) (dst mv (caddr expr))))
		((eql (car expr) 'DIV) (mvDIV mv (src mv (cadr expr)) (dst mv (caddr expr))))
		((eql (car expr) 'PUSH) (mvPUSH mv (src mv (cadr expr))))
		((eql (car expr) 'POP) (mvPOP mv (dst mv (cadr expr))))
		((eql (car expr) 'INCR) (mvINCR mv (dst mv (cadr expr))))
		((eql (car expr) 'DECR) (mvDECR mv (dst mv (cadr expr))))
		((eql (car expr) 'JMP) (mvJMP mv (dst mv (cadr expr))))
		((eql (car expr) 'CMP) (mvCMP mv (src mv (cadr expr)) (src mv (caddr expr))))
		((eql (car expr) 'JEQ) (mvJEQ mv (dst mv (cadr expr))))
		((eql (car expr) 'JL) (mvJL mv (dst mv (cadr expr))))
		((eql (car expr) 'JLE) (mvJLE mv (dst mv (cadr expr))))
		((eql (car expr) 'JG) (mvJG mv (dst mv (cadr expr))))
		((eql (car expr) 'JGE) (mvJGE mv (dst mv (cadr expr))))
		((eql (car expr) 'JNE) (mvJNE mv (dst mv (cadr expr))))
		((eql (car expr) 'JSR) (mvJSR mv (dst mv (cadr expr))))
		((eql (car expr) 'RTN) (mvRTN mv))
	)
)

;Chargement du code dans la memoire de la mv
(defun mvLOAD (mv expr)
	(let	(
			(etiqLoc (make-hash-table :size (get mv :tailleMem)))
			(etiqLocNR (make-hash-table :size (get mv :tailleMem)))
			(code expr)
		)
		(progn
			(setHash 'nb 0 etiqLocNR) 
			(loop while code 
				do
				(progn
					(cond
						((eql (caar code) '@) 
							(if (gethash (cadadr code) etiqLoc)
								(error "labels multiples :  ~s" (cadar code))
								(progn
									(setHash (cadar code) (+ (get mv :LC) 1) etiqLoc)
									(resolution mv (gethash  (cadar code) etiqLocNR) (+ (get mv :LC) 1))
									(if (gethash (cadar code) etiqLocNR)
										(progn 
											(setHash (cadar code)() etiqLocNR)
											(setHash 'nb (- (gethash 'nb etiqLocNR)  1 ) etiqLocNR)
										)
									)
								)
							)
						)
						((eql (caar code) 'JSR)
							(progn
								(if (gethash  (car (cdadar code)) (get mv :etiqGlob))
									(setMem mv (get mv :LC)
										(list (caar code) (gethash  (car (cdadar code)) (get mv :etiqGlob))))
									
									(progn  
										(if (null (gethash  (car (cdadar code)) (get mv :etiqGlobNR)))
										(setHash 'nb (+ (gethash 'nb (get mv :etiqGlobNR))  1 ) (get mv :etiqGlobNR))
										)
										(setHash (car (cdadar code)) 
											(list*  (get mv :LC) (gethash  (car (cdadar code)) (get mv :etiqGlobNR))  )
											(get mv :etiqGlobNR)
										)
										(setMem mv (get mv :LC) (car code))
									)
								)
								(mvDECR mv :LC)
							)
						)
						((eql (caar code) 'FENTRY) 
							(progn
								(if (gethash (cadadr code) (get mv :etiqGlob)) 
									(error "labels multiples : ~s" (cadadr code))
									(progn
										(setMem mv (get mv :LC) (car code))
										(setHash (cadadr code)  (get mv :LC) (get mv :etiqGlob))
										(resolution mv (gethash  (cadadr code) (get mv :etiqGlobNR)) (get mv :LC))
										(if (gethash (cadadr code) (get mv :etiqGlobNR))
											(progn 
												(setHash (cadadr code)() (get mv :etiqGlobNR))
												(setHash 'nb (- (gethash 'nb (get mv :etiqGlobNR))  1 ) (get mv :etiqGlobNR))
											)
										)
										(mvDECR mv :LC)
									)
								)
							)
						)
						((member (caar code) '(JMP JEQ JL JG JLE JGE JNE ))
							(progn
								(if (gethash  (car (cdadar code)) etiqLoc)
									(setMem mv (get mv :LC)								(list (caar code) (gethash  (car (cdadar code)) etiqLoc)))
									(progn  
										(if (null (gethash  (car (cdadar code)) etiqLocNR))
											(setHash 'nb (+ (gethash 'nb etiqLocNR)  1 ) etiqLocNR)
										)
										(setHash (car (cdadar code)) 							(list*  (get mv :LC) (gethash  (car (cdadar code)) etiqLocNR)) etiqLocNR)
										(setMem mv (get mv :LC) (car code))
									)
								)
								(mvDECR mv :LC)
							)
						)
						(t 
							(progn
								(setMem mv (get mv :LC) (car code))
								(mvDECR mv :LC)
							)
						)
					)
					(setf code (cdr code))
				)
			)
		;"Chargement du code en Memoire : OK"
		)
	)
)



;lancement du code charge en memoire
(defun mvRUN (mv)
	(progn
		(setMem mv (get mv :LC) `(HALT))
		(let	(
				(fct 0)
			)
			(loop while
				(and 
					(not (eql (car (aref (get mv :memMV) (get mv :PC))) 'HALT ))  
					(consp (aref (get mv :memMV) (get mv :PC)))
				)
				do
				(cond
					((eql (car (aref (get mv :memMV) (get mv :PC))) 'FENTRY)
						(progn 
							(setf fct (+ fct 1))
							(loop while (< 0 fct)
							do 
								(progn
									(mvDECR mv :PC)
										(cond
											((eql (car (aref (get mv :memMV) (get mv :PC))) 'FENTRY)(setf fct (+ fct 1)))
											((eql (car (aref (get mv :memMV) (get mv :PC))) 'FEXIT)(setf fct (- fct 1))))
								)
							)
						)
					)
					(t 
						(progn
							(print (aref (get mv :memMV) (get mv :PC)))
							(mvEXEC mv (aref (get mv :memMV) (get mv :PC)))
							(mvDECR mv :PC)
						)
					)
				)
			)
		)
		;(print "Resultat : ")
		(get mv :R0)
	)
)

;resolution des etiquettes
(defun resolution (mv expr adresse)
	(if (null expr) ()
		(progn
			(setMem mv (car expr) (list (car (aref (get mv :memMV) (car expr))) adresse))
			(resolution mv (cdr expr) adresse)
		)
	)
)

;Lancement de la VM
(defun mvLAUNCH (mv mem)
	; (print "Creation de la machine virtuelle ...")
	(mvMAKE mv mem)
	;(print "Initialisation de la machine virtuelle ...")
	(mvINIT mv)
	;"Machine virtuelle : OK"
)

;Restart de la vm (idem INIT)
(defun mvRESET (mv)
	;(print "Re-Initialisation de la machine virtuelle ...")
	(mvINIT mv)
	; "Machine virtuelle reinitialisee"
)

;Affiche la memoire de la vm ... nb : quand on a une suite de NIL on affiche . . . (permet de visualise toute la vm sur une 
;seulle page
(defun mvMEMVIEW (mv)
	(let	(
			(x 0)
			(rienAfficher 0)
		)
		(loop while (< x (get mv :tailleMem))
		do
			(progn
				(if (not (null (aref (get mv :memMV) x) ))
					(progn
						(if (= rienAfficher 1)
							(princ(format nil "~A:~A~%"  (- x 1) (aref (get mv :memMV) (- x 1) ) ))
						)
						(princ(format nil "~A:~A~%"  x  (aref (get mv :memMV) x) ))
						(setq rienAfficher 0)
					)
					(if (= rienAfficher 0)
						(progn 
							(princ(format nil "~A:~A~%"  x  (aref (get mv :memMV) x) ))
							(princ(format nil ".~%.~%.~%"))
							(setq rienAfficher 1)
						)
					)
				)
				(setf x (+ x 1))
			)
		)
	)
)

;Si on utilise un on fonction non implemente dans notre VM alors 
;on demande a clisp de compiler a son sour les fonctions restantes
(defun mvApply (mv fct)
	(let	
		(
			(arg (-(get mv :FP) 1 ) )
			(param   (list (aref (get mv :memMV) (-(get mv :FP) 1 ))))
		)
		(setf arg (- arg 1))
		(loop while (>= arg (- (get mv :FP) (aref (get mv :memMV) (get mv :FP))))
		do 
			(progn
				(setf  param  (cons  (aref (get mv :memMV) arg) param))
				(setf arg (- arg 1))
			)
		)
		(mvMOVE mv (apply fct param) (dst mv '(:Rx :R0)))
		(mvRTN mv)
	)
)
