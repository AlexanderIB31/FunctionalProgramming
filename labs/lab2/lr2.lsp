;; var 2.37 (level 3)
;; program was developed by Alexander Bales 80-308

(defun iterator (f arrList)
    (if (null arrList) ()
        (let ((cur (first arrList)))
            (if (not (comp-elem f cur (rest arrList)))
                    (cons (funcall f cur) (iterator f (rest arrList)))
                (iterator f (rest arrList))))))

(defun comp-elem (f iter-list cur-list)
    (cond ((null cur-list) NIL)
          ((equal (funcall f iter-list) (funcall f (first cur-list))) T)
          ((comp-elem f iter-list (rest cur-list)))))
 

(defun map-set (f x)
    (iterator f x))

(print (map-set #'identity (list 20 20 30 30 40 40)))
(print (map-set #'abs '(1 2 -3 -2)))
