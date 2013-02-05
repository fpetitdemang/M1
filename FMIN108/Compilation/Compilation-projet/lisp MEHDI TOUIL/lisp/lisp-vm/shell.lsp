(defun mvPREFETCH (mv mem)
 	;(princ "    Virtual-Machine -> PREFETCH")
	(princ "   + Virtual-Machine")
	(terpri)
	(princ "   |")
	(terpri)
	(princ "   |`-+ PREFETCH")
	(terpri)

 	;;on charge la mv
	(princ "   |  |`-> LOAD mv.lsp")
	(terpri)
 	(load "mv.lsp")

	;;on charge le compilateur
	(princ "   |  |`-> LOAD compilo.lsp")
	(terpri)
	(load "compilo.lsp")


 	;;on demarre la mv avec une memoire de 15000
	(princ "   |  |`-> LAUNCH compilo.lsp")
	(terpri)
 	(mvLAUNCH mv mem)
;;;;; (mvLOAD 'vm (compiler '(+ 1 2)))
;;;;; 	(princ (mvRUN 'vm))
	(princ "   +   `-> Virtual-Machine -> READY!")
	(terpri)

)

(let 
	(
		(TAILLE-DEFAULT 15000)
		(mv 'virtual-Machine)

	)
	(progn

		;(princ (format nil "~%-> Generating Virtual-Machine from ASM.TXT~%"))
		;mvMAKE;
		;(mvPREFETCH 'virtual-Machine (first EXT:*ARGS*))
		(mvPREFETCH mv TAILLE-DEFAULT)

		;chargment de ASM.TXT dans la VM
		;(mvOPEN 'virtual-Machine  WOOWOO)


		(terpri)
		
		(with-open-stream (stream *standard-input*)
			(princ (format nil "~%-> Please, enter your keyword~%"))
			(princ (format nil   "    \"exit\" or \"quit\" or ^D to exit ~%"))
			(princ (format nil   "    \"memview\" to display memory ~%"))
			(princ (format nil   "    \"(compiler expr)\" ex: (compiler '(+ 1 2)) ~%"))

			(princ (format nil "~%[root@virtual-machine]# " ))
			(loop for line = ( read-line stream nil) until (or (equal line "exit") (equal line "quit"))
			;(loop while  (or (equal line "exit") (equal line "quit")))

			do
				(if (< (string-width line) 1024)
					(cond 
						((equal (string-trim '(#\Space #\Tab) line) "memview")
							(mvMEMVIEW mv)
						)

						((equal (string-trim '(#\Space #\Tab) line) "1")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-addition.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))
						)

						((equal (string-trim '(#\Space #\Tab) line) "2")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-addition-setf.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "3")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-cons.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "4")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-fibo-cond.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "5")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-fibo-labels.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "6")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-fibo-lambda.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "7")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-compiler-via-compiler.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)

						((equal (string-trim '(#\Space #\Tab) line) "8")
							(mvRESET mv)
							(mvLOAD mv (compilerFichier 'example-compiler-via-mvLOAD.lsp))
							(princ(format nil "~%~%RESULTAT: ~A~%" (mvRUN mv)))

						)


						(t
							(princ (format nil "exemple inconnu"))
						)
					)
					(princ (format nil "line too big" ))
				)

	
				;(mvMEMVIEW 'virtual-Machine)
				(princ (format nil "~%[root@virtual-machine]# " ))
			)
		)
	)
	(princ (format nil "Bye :)" ))

)
