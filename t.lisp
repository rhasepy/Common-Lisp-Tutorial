(setq x 4) ; Assign
(setq y 3)

(print (sqrt (+ (* x x) (* y y)))) ; Square func

;printf("Color %s, number1 %d, number2 %05d, hex %x, float %5.2f, unsigned value %u.\n",
;"red", 123456, 89, 255, 3.14, 250);
;(format t "Color ~A, number1 ~D, number2 ~5,'0D, hex ~X, float ~5,2F, unsigned value ~D.~%"
;"red" 123456 89 255 3.14 250)

(setq ka "bilgisayar kavramlari")
(print ka)

(print (mod 11 5))
(print (rem 11 5))
(print (incf x 5)) ; incf increment operator
(print (decf x 5)) ; decf decrement operator
(print (max x y)) ; return max operator
(print (min x y)) ; return min operator

;;;;;;;;;;;;;;;FLOW CHART;;;;;;;;;;;;;;;;;;;;
(if (= 2 1) ; If state
	(print "Esit") ; State of True
	(print "Esit Degil")) ; State of False

(if (= x y) ; If state
	(print "x ve y esit")
	(if (< x y) ; Else if state
		(print "x y den kucuk")
		(print "x y den buyuk"))) ; Else state

; Condition step
(cond ( (= x y) (print "x ve y esit") ) 
	( (< x y) (print "x y den kucuk") )
	( (> x y) (print "x y den buyuk") ))
;;;;;;;;;;;;;;;FLOW CHART;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;
(defun f (a) 
	(* 2 a))

(print (f x))

(defun fact (n) 
	(if (= n 0) 1
	(* n (fact (- n 1)))))

(print (fact 5))

(defun fibo (n)
	(if (= n 1) 1
		(if (= n 2) 1
			(+ (fibo (- n 1)) (fibo (- n 2)) ))))

(print (fibo 6))
;;;;;;;;;;;;;;;;FUNCTIONS;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;LOOP;;;;;;;;;;;;;;;;;;;;;;;
(terpri)
(terpri)
(setq a 10)
(loop
	(setq a (+ a 1))
	(write a)
	(terpri)
	(when (> a 17) (return a)))

(terpri)
(terpri)
(loop for a from 10 to 20 ; 10 dan baslar 20 dahil
	do (print a))

(terpri)
(terpri)
(loop for a from 1 to 20
	if (evenp a)
	do (print a))

(terpri)
(terpri)
(loop for a in '(tom dick harry) ; liste bosluga gore parse ediliyor
   do (print a))

(terpri)
(terpri)
(dotimes (n 11) ; 0 dan 10 a kadar 10 dahil
	(print n) (prin1 (* n n))) ; prin1 bosluk koyar

(terpri)
(terpri)
(dolist (n '(1 2 3 4 5 6 7 8 9))
   (format t "~%Number: ~d Square: ~d" n (* n n))) ; print for format
;;;;;;;;;;;;;;;;LOOP;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;LIST;;;;;;;;;;;;;;;;;;
(terpri)
(defparameter my-list (list 1 2 3)) ; listeyi define etmek
(print my-list)
(print (first my-list)) ; ilk eleman
(print (car my-list)) ; ilk eleman
(print (cdr my-list)) ; ilk eleman hariç olan liste

(defparameter my-list (append (list 10) my-list)) 
(print my-list)
(defparameter my-list (cons (list 20) my-list))
; cons liste olarak ekler alt liste gibi eger liste varsa
; tek varsa tek ekler
; appendden farkı append tek tek tum listeyi pushlar
; cons alt liste olarak pushlar
; ikisinde de define etmek gerekir
(print my-list)
;;;;;;;;;;;;;;;;LIST;;;;;;;;;;;;;;;;;;