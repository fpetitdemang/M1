(defun fact1 (n) (if (= n 0) 1 ( * n ( fact1 (- n 1 )))))
  
(defun fact2 (n r) (if (= n 0) r (fact2 ( - n 1 ) ( * n r))))

(defun fact3 (n) (labels ((fact-t (x r) (if (= x 0) r (fact-t ( - x 1 ) ( * x r))))) (fact-t n 1)))
 