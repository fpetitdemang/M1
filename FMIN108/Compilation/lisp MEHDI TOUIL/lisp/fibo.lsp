(defun fibo(n)
  (if ;(<= n 1);
      (or (= n 0) (= n 1))
      1
    (+ (fibo(- n 1))(fibo (- n 2)))))
