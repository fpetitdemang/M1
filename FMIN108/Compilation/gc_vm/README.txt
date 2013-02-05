
;################################
;##   Projet de compilation    ##
;##         2009/2010          ##
;##                            ##
;##    Réalisé par:            ##
;##    ------------            ##
;##    EL ASRI Mohamed         ##
;##    BOUZROUD Khalid         ##
;##    EL AYACHI Yassine       ##
;################################

;- meval.lisp	: Le méta-évaluateur
;- gc.lisp		: Le générateur de code (compilateur)
;- vm.lisp		: La machine virtuelle
;- tests		: Le répertoire qui contient les exemples sur lesquels déroule les tests
;- asm			: Le répertoire dont le code pseudo-assembleur sera généré par le compilateur

;###############################
;##         SOMMAIRE          ##
;###############################
#|
	- Le méta-évaluateur
		1) Le méta-évaluateur
			Tests: simple
			Tests: fonctions méta-définis
		2) méta-évaluation de méta-évaluateur
	- Le générateur de code (Compilateur)
		Tests: simple
		Tests: sur des fonctions
		Tests: gc de gc
	- La machine virtuelle
		Execution
		La deuxième instance de la machine virtuelle
|#
;###############################
;##    Le méta-évaluateur:    ##
;###############################

;;;; 1) Le méta-évaluateur

; Chargement de méta-évaluateur
(load "meval.lisp")

; Tests: simple
(meval '(+ 1 2) ())
(meval 'a '((a 4)))
(meval '((lambda (x) x) 3) ())
(meval '(defun foo (x) (+ x 6)) ())

; Tests: fonctions méta-définis

(mload "tests/fibo.lisp")
(meval '(fibo 10) ())

(mload "tests/fact.lisp")
(meval '(fact 5) ())

;;;; 2) méta-évaluation de méta-évaluateur

; Chargement de méta-évaluateur (si n'est pas fait)
(load "meval.lisp")

; méta-Chargement de méta-évaluateur
(mload "meval.lisp")

(meval '(meval '(+ 1 2) ()) ())

(mload "tests/fibo.lisp")
(meval '(meval '(fibo 6) ()) ())

(mload "tests/fact.lisp")
(meval '(meval '(fact 7) ()) ())


;################################################
;##    Le générateur de code (Compilateur):    ##
;################################################

; Chargement de générateur de code
(load "gc.lisp")

; Tests: simple
(gc-exec '(+ 1 2))
(gc-exec '(+ 1 2) "asm/result.asm")

; Tests: sur des fonctions
(gcload "tests/fibo.lisp" "asm/fibo.asm")
(gcload "tests/arithmetique.lisp" "asm/arithmetique.asm")
(gcload "tests/arithmetique-setf.lisp" "asm/arithmetique-setf.asm")
(gcload "tests/test-cons.lisp" "asm/test-cons.asm")
(gcload "tests/fibo-cond.lisp" "asm/fibo-cond.asm")
(gcload "tests/fibo-labels.lisp" "asm/fibo-labels.asm")

; Tests: gc de gc
(gc-exec '(gc-exec '(+ 1 2)))
(gc-exec '(gcload "tests/arithmetique.lisp") "asm/gc_gc_arithmetique.asm")

;#################################
;##    La machine virtuelle:    ##
;#################################

;;;; 1) Execution

; chargement de générateur de code 
(load "gc.lisp")

; Chargement/création de la machine virtuelle
(load "vm.lisp")
(vm-make 'vmtest 20000)

; execution de (+ 1 2)
(vm-init 'vmtest)
(vm-run 'vmtest (gc-exec '(+ 1 2)) nil)

; execution de "fibo-cond" [avec appel interne]
(vm-init 'vmtest)
(vm-run 'vmtest (gcload "tests/fibo-cond.lisp") nil)

; execution de "fibo" [avec appel "externe"]
; NB. la compilation de l'appel est interne
(vm-init 'vmtest)
(vmloader 'vmtest (gcload "tests/fibo.lisp"))
(vm-exec-gc 'vmtest '(fibo 5) nil)

; même chose: execution de "fibo" [avec appel "externe"]
; mais avec une pré-compilation de l'appel
(vm-init 'vmtest)
(vmloader 'vmtest (gcload "tests/fibo.lisp"))
(vm-exec 'vmtest (gc-exec '(fibo 5)) nil)

; Test de "let"
(vm-init 'vmtest)
(vmloader 'vmtest (gcload "tests/let.lisp"))
(vm-exec-gc 'vmtest '(counter) nil)
(vm-exec-gc 'vmtest '(counter++) nil)
(vm-exec-gc 'vmtest '(counter-reset) nil)

;;;; 2) La deuxième instance de la machine virtuelle

(vm-make 'vmtest2 20000)
(vm-run 'vmtest2 (gc-exec '(+ 1 2)) nil)
