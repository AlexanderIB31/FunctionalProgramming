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

(defun find-word (word source)
    (let ((listWords (word-list source))
          (cntSentences 0)
          (cntWords 0))
        (counter word listWords (list cntWords cntSentences))))

(defun counter (word listWords listPos)    
    (if (null listWords) NIL
        (if (string-equal word (first listWords)) (values (first listPos) (second listPos))
            (counter word (rest listWords) (checkAndSetCnt (first listWords) (first listPos) (second listPos))))))

(defun string-equal (s1 s2)
    (if (null (loop for i upfrom 0 below (min (length s1) (length s2)) 
              do (if (not (equal (char s1 i) (char s2 i))) (return NIL)
                      t) (return t))) NIL
        (if (or (equal (length s1) (length s2)) 
              (and (equal (1- (length s2)) (length s1)) (textspace-char-p (char s2 (1- (length s2)))))) t
            NIL)))

(defun checkAndSetCnt (word cntW cntS)
    (if (equal (char word (1- (length word))) #\.) (list 0 (1+ cntS))
        (list (1+ cntW) cntS)))