#|

  TER 141 2005-2006
  Fichier test de generation
  test.lsp

  Lebec Laurie : laurie.lebec@gmail.com
  Leture Julien : julien@leture.com

|#


; codes que la fonction (compiler-fichier "test.lsp") va lire
; >> retourne le code assembleur genere correspondant

3
(+ 2 3)
(+ (* 5 2) (+ 2 (- 2 3)))
(toto 1 2 (titi 3 4))
(if (< a 5) 1 2)
(defun toto (a b c d) (if (< a b) (tata c) (titi d)))