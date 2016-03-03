;; var 1.41 (level 4)
;; program was developed by Alexander Bales 80-308

(defun pow (x y)
    (cond 
        ((= y 0.0) 1.0)
        ((= y 1.0) x)
        ((> y 1.0) (* x (pow x (- y 1.0))))
    )
)    

(defun sine (x)
    (if (> x 0.1) 
        (- (* 3.0 (sine (/ x 3.0))) (* 4.0 (pow (sine (/ x 3.0)) 3.0)))
        x
    )
)

(defun main ()
    (sine 12.15)
)
