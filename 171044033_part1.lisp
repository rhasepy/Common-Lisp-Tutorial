(defun flatten (li)
  (cond ((null li) nil)
        ((atom li) `(,li) ) ;;THIS CONFUSES ME :(
        (t (mapcan #'flatten li))))

(defun flattens (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun flattenn ( l )
    (if (atom l)
        (list l)
        (append (flatten (car l)) (if (cdr l) (flatten (cdr l))))))

(defun readFile()
	(let 
		((in (open "nested_list.txt" :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      while line do (format t "~a~%" line))
      (close in))))

(readFile)
(print (flattenn `(a (a b) (a b (a c)))))