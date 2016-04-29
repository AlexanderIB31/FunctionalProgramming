(defun mult-matrix (A B)
	(setf matrixRes (make-array '((array-dimension A 0) (array-dimension B 1)) :initial-element '(float 0.0))))
	(loop with sz1 = (array-dimension A 0)
		for i upfrom 0 below sz1 do
			(loop with sz2 = (array-dimension A 1)
				for j upfrom 0 below sz2 do
					(loop with sz3 = (array-dimension B 1)
						for k upfrom 0 below sz3 do
							
						
(defun task (matrixA matrixB)
	(loop with n = (array-dimension matrixA 0)
		for i upfrom 0 below n do
			(loop with szRow = (array-dimension matrixA i)
				for j upfrom 0 below szRow do
					(cond ((= i j) (setf (aref matrixB i j) 0))
						((< i j) (setf (aref matrixB i j) (/ 1.0 (i + j - 1))))
						(setf [matrixB i j] (/ (- 1.0) (i + j - 1))))))
	(mult-matrix matrixA matrixB))

(defun start (matrixA)
	(let ((matrixB (make-array '(n n))))
		(task matrixA matrixB)))

(defun print-matrix (matrix &optional (chars 3) stream)
  (let ((*print-right-margin* (+ 6 (* (1+ chars) 
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))

(defun main ()
	(let ((matrixA (make-array '(3 3) :initial-contents '((0.0 1.0 2.0) (3.0 4.0 5.0) (6.0 7.0 8.0)))))
		(start matrixA)))
