;; var 2.37 (level 3)
;; program was developed by Alexander Bales 80-308

(defun iterator (arrList)
    (if (null arrList) ()
        (let ((cur (first arrList)))
            (if (not (comp-elem cur (rest arrList)))
                    (cons cur (iterator (rest arrList)))
                (iterator (rest arrList))))))

(defun comp-elem (elem cur-list)
    (cond ((null cur-list) NIL)
          ((equal elem (first cur-list)) T)
          ((comp-elem elem (rest cur-list)))))
 
(defun genList (f arr)
    (if (null arr) ()
        (cons (funcall f (first arr)) (genList f (rest arr)))))

(defun map-set (f x)
    (iterator (genList f x)))

(print (map-set #'identity (list 20 20 30 30 40 40)))
(print (map-set #'abs '(1 2 -3 -2)))
