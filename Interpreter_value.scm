;OPERATOR/OPERAND PARSING FUNCTIONS==============================
;FORMAT: (operator operand1 operand2)
; moved to interpreter.rkt

(define value
  (lambda (ex e)
    (cond
      ((not (list? ex)) 
       (cond 
         ((or (boolean? ex) (number? ex)) ex)
         ((null? (lookup ex e)) (error 'value "Variable does not exist in scope"))
         (else (lookup ex e))))
      (else ((value-f ex e) ex e)))))

(define value-f
  (lambda (ex e)
    (cond
      ((eq? (operator ex) '=) assign-stmt)
      ((eq? (operator ex) 'funcall) func-call)
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
    (lambda (ex e)
      (cond
        ((and (eq? (operator ex) '-) (eq? (length ex) 2)) (- 0 (value (operand1 ex) e)))
        ((eq? (operator ex) '!) (not (value (operand1 ex) e)))
        (else (f (value (operand1 ex) e) (value (operand2 ex) e)))))))
      