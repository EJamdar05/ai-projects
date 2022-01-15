;Eshaq Jamdar
;Nabin Chaudhary
;Giang Tran

;sums up every elemen in list 
;used for the terminal test
(defun SUM-HELPER (numbers)
    (if (not (null numbers))  (+ (car numbers) (SUM-HELPER (cdr numbers)))0)
)
;terminal test will take a list, use a helper sum function
;to determine if every element is 0 in the list 
;since a terminal test checks if all elements are 0
(defun TERMINAL-TEST (list)
  (setq total (SUM-HELPER list))
  (eq 0 total)
)
(print " 1. TERMINAL-TEST ")
(terpri)
(write(TERMINAL-TEST '(0 0 0 0)))
(terpri)
(write(TERMINAL-TEST '(1 0 0 0)))
(terpri)


;binary translator that will take in a list 
;and total (which represents 3 bits)
;the translation will happen from the most significant
;bit over to the least significant and translate it
;over to base 10
(defun UTILITY (a total)
    (setq number 0)
    (loop for x in a
        if(eq 1 x)
        do (incf number (+(expt 2 total))) (decf total 1)
        if (eq 0 x)
        do (decf total 1)
)
(+ number 0)
)

(terpri)(terpri)
(print " 2. UTILITY ")
(print(UTILITY '(0 0 0 0) 3))
(print(UTILITY '(1 0 0 0) 3))
(print(UTILITY '(1 1 1 1) 3))

;result function takes in a list of states and a number representing action
;based off the first two variables
;list will be returned as the result
(defun RESULT (list action)
    (setq var1 (car list))
    (setq var2 (car (cdr list)))
    (cond
        ((and (eq var1 0) (eq var2 0) (eq action 0) list))
        ((and (eq var1 0) (eq var2 0) (eq action 1) '(0 0 0 1)))
        ((and (eq var1 1) (eq var2 0) (eq action 2) '(1 0 1 0)))
        ((and (eq var1 1) (eq var2 1) (eq action 3) '(1 1 1 1)))
        ((and (eq var1 1) (eq var2 1) (eq action 4) list))
    )
)


(terpri)(terpri)
(print " 3. RESULT ")
(print (RESULT '(0 0 0 0) 0))      ; 0 0 0 0
(print (RESULT '(0 0 0 0) 1))      ; 0 0 0 0
(print (RESULT '(1 0 1 0) 2))      ; 1 0 1 0
(print (RESULT '(1 1 1 1) 3))      ; 1 1 1 1
(print (RESULT '(1 1 0 0) 4))      ; 1 1 0 0

;actions takes in a list of states
;and based off the first two elements in the list
;we will return an action based off the state
(defun ACTIONS (list)
    (setq total (SUM-HELPER list))
    (setq var1 (car list))
    (setq var2 (car (cdr list)))
    (cond
        ((and (eq total 0)  '(0 1)))
        ((and (eq var1 0) (eq var2 1)  1))
        ((and (eq var1 1) (eq var2 0)  2))
        ((and (eq var1 1) (eq var2 1)  '(3 4)))
    )
)

(terpri)(terpri)
(print " 4. ACTIONS ")
(print (ACTIONS '(0 0 0 0)))
(print (ACTIONS '(0 1 0 0)))
(print (ACTIONS '(1 0 0 0)))
(print (ACTIONS '(1 1 0 0)))


