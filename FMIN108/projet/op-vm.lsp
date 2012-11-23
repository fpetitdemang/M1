(defun my-LOAD (src dest);memoire->registre
  (setf dest src))

(defun my-STORE (src dest);registre->memoire
  (setf src dest))

(defun MOVE (src dest);registre->registre
  (my-LOAD src dest))

(defun ADD (src dest);ajoute src+dest dans dest
  (setf dest (+ src dest)))
  
