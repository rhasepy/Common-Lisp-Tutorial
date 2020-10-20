(defun printCollatz (number)

	(format t "~d: " number)

	(loop while (not (= number 1))
		do
			(format t "~d " number)
			(if (= 1 (mod number 2)) 
				(setq number (+ (* 3 number) 1))
				(setq number (/ number 2))))
	(format t "~d~%" number))

(defun readFile()

	(setq my-list '())

	(let ((in (open "boundries.txt" :if-does-not-exist nil)))
	   (when in
	      (loop for token = (read in nil)
	      while token do (setq my-list (append my-list (list token))))
	      (close in)))

	my-list)

(defun main()
	(printCollatz 17))

(main)