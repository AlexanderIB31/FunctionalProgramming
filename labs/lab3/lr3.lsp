(defun mult-matrix (A B)
	(let ((matrixRes (make-array (list (array-dimension A 0) (array-dimension B 1)) 
			:initial-element 0.0
			:element-type 'single-float)))
		(loop with sz1 = (array-dimension A 0)
			for i upfrom 0 below sz1 do
				(loop with sz2 = (array-dimension A 1)
					for j upfrom 0 below sz2 do
						(let ((temp 0))
							(loop with sz3 = (array-dimension B 0)
								for k upfrom 0 below sz3 do
									(setq temp (+ temp (* (aref A i k) (aref B k j)))))
							(setf (aref  matrixRes i j) temp))))
	matrixRes))
						
(defun task (matrixA matrixB)
	(loop with n = (array-dimension matrixA 0)
		for i from 1 to n do
			(loop with szRow = (array-dimension matrixA 1)
				for j from 1 to szRow do
					(cond ((= i j) (setf (aref matrixB (- i 1) (- j 1)) 0.0))
						((< i j) (setf (aref matrixB (- i 1) (- j 1)) (/ 1.0 (+  i (- j 1)))))
						(t (setf (aref matrixB (- i 1) (- j 1)) (/ -1.0 (+ i (- j 1))))))))
	(mult-matrix matrixA matrixB))

(defun start (matrixA)
	(let ((matrixB (make-array (list (array-dimension matrixA 0) (array-dimension matrixA 1)))))
		(task matrixA matrixB)))

(defun init-matrix (matr cntRow cntCol)
	(let ((val 0.0))
		(loop for i upfrom 0 below cntRow do
			(loop for j upfrom 0 below cntCol do
				(setf (aref matr i j) (setq val (1+ val))))))
	(values))
		

(defun main (n)
	(let ((matrixA (make-array (list n n) 
			:element-type 'single-float)))
		(init-matrix matrixA n n)
		(PrintMatrix (start matrixA))))

(defun GetListFromMatrix (A row curPos len)
	(if (>= curPos len) ()
		(cons (aref A row curPos) (GetListFromMatrix A row (1+ curPos) len))))

(defun PrintMatrix (A)
	(loop with n = (array-dimension A 0)
		for i upfrom 0 below n do
			(pprint (GetListFromMatrix A i 0 n))))
