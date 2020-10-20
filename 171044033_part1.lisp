(defun myFlatten (listParam &optional (emptylist '()))

	(loop for item in listParam
		do
			(if (atom item)
				(setq emptylist (append emptylist (list item)))
				(setq emptylist (myFlatten item emptylist))))
	emptylist)

(defun readFile()

	(setq my-list '())
	(let 
		((in (open "nested_list.txt" :if-does-not-exist nil)))
   (when in
      (loop for coin = (read in nil)
      while coin do (setq my-list (append (list coin) my-list)))
      (close in)))
	(reverse my-list))

(defun main()

 (with-open-file (out "flattened_list.txt" 
 	:direction :output 
 	:if-does-not-exist :create)

    (dolist (segment (myFlatten (readFile)))
      (format out "~D " segment))
    (format out "~%")))

(main)