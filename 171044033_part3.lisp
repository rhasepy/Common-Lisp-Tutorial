(defun printCollatz (number)

	(format t "~d: " number)

	(loop while (not (= number 1))
		do
			(format t "~d " number)
			(if (= 1 (mod number 2)) 
				(setq number (+ (* 3 number) 1))
				(setq number (/ number 2))))
	(format t "~d~%" number))

(printCollatz 17)