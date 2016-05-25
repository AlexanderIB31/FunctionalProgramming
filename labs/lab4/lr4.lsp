(defun whitespace-char-p (char)
    (member char '(#\Space #\Tab #\Newline)))

(defun textspace-char-p (char)
    (member char '(#\, #\. #\; #\! #\?)))

(defun word-list (string)
    (loop with len = (length string)
          for left = 0 then (1+ right)
          for right = (or (position-if #'whitespace-char-p string
                                       :start left)
                          len)
          unless (= right left)
            collect (subseq string left right)
          while (< right len)))

(defun strip (string)
    (loop with len = (length string)
          for left = 0 then (1+ right)
          for right = (or (position-if #'textspace-char-p string
                                       :start left)
                          len)
          unless (= right left)
            return (subseq string left right)))


(defun find-word-with-id (word source pos)
	(if (null source) NIL
	    (let* ((listWords (word-list (first source)))
	          (cntWords 0)
	          (listAnsw (counter word listWords (list cntWords pos))))
	        (if (null listAnsw) 
	        	(find-word-with-id word (rest source) (1+ pos))
	        	(values (first listAnsw) (second listAnsw))))))

(defun find-word (word source)
	(find-word-with-id word source 0))

(defun counter (word listWords listPos)    
    (if (null listWords) NIL
        (if (string-equal word (strip (first listWords))) (list (first listPos) (second listPos))
            (counter word (rest listWords) (checkAndSetCnt (first listWords) (first listPos) (second listPos))))))

(defun checkAndSetCnt (word cntW cntS)
    (if (equal (char word (1- (length word))) #\.) (list 0 (1+ cntS))
        (list (1+ cntW) cntS)))
