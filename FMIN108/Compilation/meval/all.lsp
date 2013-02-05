; ***** CHARGEUR METAEVAL *****

(print "***** Définition + méta-définition des environnements *****")
(load "env.lsp")

(setf lstfctenv (global ensfct))

(print "***** Définition + méta-définition du méta-évaluateur *****")
(load "eval.lsp")

; * métaévalue le contenu d'un fichier

(defun mload (fic)
  (labels ((aux (X)
		(let ((r (read X nil nil nil)))
		  (if r
		    (progn (meval r) (aux X))
		    T))))
    (aux (open fic))))

(setf lstfctmeval (global ensfct))
(setf ensfct '(()()))

(print "***** Définition + méta-définition des automates *****")
(mload "auto.lisp")

(setf lstfctauto (global ensfct))
(setf ensfct '(()()))

(print "***** Définition + méta-définition du générateur de code *****")

(load "gen.lisp")
(load "gen_env.lisp")
(load "gen_utils.lisp")
(load "gen_proto.lisp") 
(load "utils_functions.lisp") 
(load "gen_vm.lisp")
(mdefun vm-compile-function-list (lst) (mapcar #'vm-compile-function lst))


(setf lstfctcomp (global ensfct))
(setf ensfct '(()()))

(print "***** Définition + méta-définition de la machine virtuelle *****")
(load "power.lsp")
(load "mem.lsp")
(load "mem-pile.lsp")

(setf lstfctmv (global ensfct))
(setf ensfct '(()()))



(setf (cadr ensfct) (append (copy-tree lstfctmeval) (copy-tree lstfctcomp) (copy-tree lstfctauto) (copy-tree lstfctmv)))

(setf lstfctmeval (mapcar #'car lstfctmeval))
(setf lstfctauto (mapcar #'car lstfctauto))
(setf lstfctmv (mapcar #'car lstfctmv))
(setf lstfctcomp (mapcar #'car lstfctcomp))
(setf lstfctenv (mapcar #'car lstfctenv))

()