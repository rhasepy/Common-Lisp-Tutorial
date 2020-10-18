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

(defun recognizer (number)

	(if (= 1 (isPrime number)) (format t "~d is Prime~%" number)
		(if (= 1 (isSemiPrime number))
			(format t "~d is Semi-prime~%" number))))

(loop for a from 2 to 10
	do (recognizer a))