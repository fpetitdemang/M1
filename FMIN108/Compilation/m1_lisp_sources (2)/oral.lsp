#|

  TER 141 2005-2006
  Demonstration orale
  oral.lsp

  Lebec Laurie : laurie.lebec@gmail.com
  Leture Julien : julien@leture.com

|#


; CHARGEMENT
; chargement des 2 fichiers
(load "gencode.lsp") ; (load "~/Desktop/gencode.lsp")
(load "mv.lsp") ; (load "~/Desktop/mv.lsp")


; GENERATION DE CODE
; generation de code sur un fichier exemple
(compiler-fichier "test.lsp") ; (compiler-fichier "~/Desktop/test.lsp")


; MACHINE VIRTUELLE
; fonction carre
(vm-make) ; ie. (vm-make :nom 'VM1 :taille 10000)
(vm-load (compiler '(defun carre (x) (* x x))) 'VM1)
(vm-apply 'carre 'VM1 3)


; fonction fact enveloppee
(vm-make)
(vm-load (compiler '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) 'VM1)
(vm-apply 'fact 'VM1 5)


; fonction fact terminale
(vm-make)
(vm-load (compiler '(defun factt (n p) (if (<= n 1) p (factt (- n 1) (* n p))))) 'VM1)
(vm-apply 'factt 'VM1 5 1)


; fonction fibo naive
(vm-make)
(vm-load (compiler '(defun fibo (n) (cond ((= n 0) 1) ((= n 1) 1) (T (+ (fibo (- n 1)) (fibo (- n 2))))))) 'VM1)
(vm-apply 'fibo 'VM1 6)
; ie. (vm-load (compiler '(defun fibo (n) (if (<= n 1) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) 'VM1)


; creation d'une variable globable
(vm-make)
(vm-load (compiler '(setf a 3)) 'VM1)
(vm-load (compiler '(+ a 2)) 'VM1)
(vm-run 'VM1)


; test des etiquettes
; > sans les ref avant
(vm-make)
(vm-load '((TOTO) (RTN) (JSR (AT TOTO))) 'VM1)
(aff-memory-code 'VM1)

; > avec les ref avant
(vm-make)
(vm-load '((JSR (AT TOTO)) (TOTO) (RTN)) 'VM1)
(aff-memory-code 'VM1)

; > avec une erreur dans les ref avant
(vm-make)
(vm-load '((JSR (AT TOTO)) (TATA) (RTN)) 'VM1)
(aff-memory-code 'VM1)
(get-ref 'VM1 'TOTO)

; appel d'une fonction avant sa definition
(vm-make)
(vm-load (append (compiler '(factt 5 1)) (compiler '(defun factt (n p) (if (<= n 1) p (factt (- n 1) (* n p)))))) 'VM1)
(vm-run 'VM1)
