;; var 1.41 (level 4)
;; program was developed by Alexander Bales 80-308

(defun cube (x)
    (* x x x)
)    

(defun sine (x)
    (if (or (> x 0.1) (< x -0.1)) 
        (- (* 3.0 (sine (/ x 3.0))) (* 4.0 (cube (sine (/ x 3.0)))))
        x
    )
)

(defun main ()
    (format t "sin(12.15) = ~3$~%" (sine 12.15))
    (format t "sin(pi) = ~3$~%" (sine pi))
    (format t "sine(pi/2) = ~3$~%" (sine (/ pi 2)))
    (format t "sine(-pi/2) = ~3$~%" (sine (/ (- pi) 2)))
    (format t "sine(-pi/6) = ~3$~%" (sine (/ (- pi) 6)))
)    

