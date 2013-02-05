(defun longueur (list) (if (eq () list) 0 (+ 1 (longueur (cdr list)))))

(defun longueur-t ( list acc ) ( if (eq () list) acc (longueur-t (cdr list) (+ acc 1))))

(defun membre (x liste) (if (atom liste) liste (if (eq (car liste) x) liste (membre x (cdr liste)))))

(defun dernier (list) (if (consp (cdr list)) (dernier (cdr list)) (car list)))

(defun creerListeCBis (n acc) (if (= 0 n) () (cons acc (creerListeCBis (- n 1) (+ acc 1)))))

(defun creerListeD (n) (if (= n 0) () (cons n (creerListeD (- n 1)))))

(defun supprime (x liste) (if (atom liste) liste (if (eq x (car liste)) (supprime x (cdr liste)) (cons (car liste) (supprime x (cdr liste))))))

(defun concat (list1 list2) (if (atom (cdr list1)) (cons (car list1) list2) (cons (car list1) (concat (cdr list1) list2))))

(defun adjoint (x liste) (if (atom liste) (cons x liste) (if (eq (car liste) x) liste (cons (car liste) (adjoint x (cdr liste)))))) 

(let ((etiq 0)) (defun compteur () etiq) (defun compteur++ () (setf etiq (+ etiq 1))) (defun reset () (setf etiq 0)))