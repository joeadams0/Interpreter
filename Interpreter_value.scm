;OPERATOR/OPERAND PARSING FUNCTIONS==============================
;FORMAT: (operator operand1 operand2)
; moved to interpreter.rkt

(define value
  (lambda (ex e class instance)
    (cond
      ((not (list? ex)) 
       (cond 
         ((or (boolean? ex) (number? ex)) ex)
         ((eq? ex 'true) #t)
         ((eq? ex 'false) #f)
         ((null? (lookup-var ex class instance e #f)) (error 'value "Variable does not exist in scope"))
         (else (lookup-var ex class instance e #f))))
      ((eq? (operator ex) 'dot) (dot-var-lookup ex e class instance #f))
      ((eq? (operator ex) 'new) (new-instance (car (cdr ex)) e))
      (else ((value-f ex e) ex e class instance)))))

(define value-f
  (lambda (ex e)
    (cond
      ((eq? (operator ex) '=) assign-stmt)
      ((eq? (operator ex) 'funcall) method-call)
      ((eq? (operator ex) '+) (math-f +))
      ((eq? (operator ex) '-) (math-f -))
      ((eq? (operator ex) '/) (math-f quotient))
      ((eq? (operator ex) '*) (math-f *))
      ((eq? (operator ex) '%) (math-f remainder))
      ((eq? (operator ex) '==) (math-f eq?))
      ((eq? (operator ex) '!=) (math-f (lambda (a b) (not (eq? a b)))))
      ((eq? (operator ex) '>) (math-f >))
      ((eq? (operator ex) '<) (math-f <))
      ((eq? (operator ex) '>=) (math-f >=))
      ((eq? (operator ex) '<=) (math-f <=))
      ((eq? (operator ex) '!) (math-f not))
      ((eq? (operator ex) '&&) (math-f (lambda (a b) (and a b))))
      ((eq? (operator ex) '||) (math-f (lambda (a b) (or a b))))
      (else (error 'value "Invalid Expression")))))

(define math-f
  (lambda (f)
    (lambda (ex e class instance)
      (cond
        ((and (eq? (operator ex) '-) (eq? (length ex) 2)) (- 0 (value (operand1 ex) e class instance)))
        ((eq? (operator ex) '!) (not (value (operand1 ex) e class instance)))
        (else (f (value (operand1 ex) e class instance ) (value (operand2 ex) e class instance)))))))
      