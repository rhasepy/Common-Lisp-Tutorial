(defun isPrime (number &optional (divider 2))

	(if (= 2 number) 1
		(if (< number 2) 0
			(if (= 0 (mod number divider)) 0 
				(if (> (* divider divider) number) 1
				(isPrime number (incf divider)))))))

(defun isSemiPrime (number)

	(setq Pcounter 0)
	(setq Lcounter 2)

	(loop while (and (< Pcounter 2) (<= (* Lcounter Lcounter) number))
		do 
		(loop while (= 0 (mod number Lcounter))
			do 
				(setq number (/ number Lcounter))
				(incf Pcounter))
		(incf Lcounter))

	(if (> number 1) (incf Pcounter))
	(if (= 2 Pcounter) 1 0))

(defun readFile()

	(setq my-list '())

	(let ((in (open "boundries.txt" :if-does-not-exist nil)))
	   (when in
	      (loop for token = (read in nil)
	      while token do (setq my-list (append my-list (list token))))
	      (close in)))

	my-list)

(defun main()

	(setq numberList (readFile))
	(setq lowerBound (min (car numberList) (car (cdr numberList))))
	(setq upperBound (max (car numberList) (car (cdr numberList))))

	 (with-open-file (out "primedistribution.txt"
	 	:direction :output 
	 	:if-does-not-exist :create)

		(loop for number from lowerBound to upperBound
			do 
			(if (= 1 (isPrime number)) (format out "~d is Prime~%" number)
				(if (= 1 (isSemiPrime number)) (format out "~d is Semi-prime~%" number))))))

(main)