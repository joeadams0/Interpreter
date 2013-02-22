;OPERATOR/OPERAND PARSING FUNCTIONS==============================
;FORMAT: (operator operand1 operand2)
;returns the car of the cdr of a list. Assumes operator is in front of operands.
(define operator
  (lambda (ex)
    (car ex)))
;returns the car of a list
(define operand1
  (lambda (ex)
    (car (cdr ex))))
;returns the cdr of the cdr of a list
(define operand2
  (lambda (ex)
     (cdr (cdr ex))))
;END OPERATOR/OPERAND PARSING FUNCTIONS==============================

;EXPRESSION CHECKING FUNCTIONS===============================
;checks if the list passed in is any kind of expression, math, predicate, or boolean.
(define expression?
  (lambda (ex e)
    (cond
      ((math? ex e) #t)
      ((predicate? ex e) #t)
;      ((bool? ex e) #t)
      (else #f))))
;returns #t if the list is a valid math expression, #f otherwise
(define math?
  (lambda (ex e)
    (cond
      ((null? ex) #f)
      ((number? ex) #t)
      ((not (pair? ex)) (number? (lookup ex e)))
      ((null? (cdr ex)) (math? (car ex) e))
      ((not (= (length ex) 3)) 
       (cond
         ((and (math? (cons (car ex) (cons (car (cdr ex)) (cons(car (cdr (cdr ex))) '()))) e)
               (math? (cons (car(cdr(cdr ex))) (cdr (cdr(cdr ex)))) e)))
         (else #f)))
      ((or (eq? '= (operator ex)) (eq? '+ (operator ex)) (eq? '- (operator ex)) (eq? '* (operator ex)) (eq? '/ (operator ex)) (eq? '% (operator ex)))(and (math? (operand1 ex) e) (math? (operand2 ex) e)))
      (else #f))))
;returns #t if the list is a valid predicate function, #f otherwise
(define predicate?
  (lambda (ex e)
    (cond
      ((null? ex) #f)
      ((number? ex) #f)
      ((not (pair? ex)) #f)
      ((null? (cdr ex)) (predicate? (car ex)e))
      ((not (= (length ex) 3)) #f)
      ((or (eq? '== (operator ex)) (eq? '!= (operator ex)) (eq? '> (operator ex)) (eq? '< (operator ex)) (eq? '>= (operator ex)) (eq? '<= (operator ex)))(and (expression? (operand1 ex)e) (expression? (operand2 ex)e)))
      (else #f))))
;returns #t if the list is a valid boolean function, #f otherwise
(define bool?
  (lambda (ex e)
    (cond
      ((null? ex) #f)
      ((not (pair? ex))(if
                        (or (eq? (lookup ex e) #t)(eq? (lookup ex e) #f)) #t
                        #f))
      ((null? (cdr ex)) (bool? (car ex)))
      ((not (= (length ex) 3)) #f)
      ((or (eq? '! (operator ex)) (eq? '&& (operator ex)) (eq? '|| (operator ex)))(and (bool? (operand1 ex)) (bool? (operand2 ex))))
      (else #f))))
;END EXPRESSION CHECKING FUNCTIONS===============================

;THE EVALUATOR FUNCTIONS=========================================
;the main thing the rest of the interpreter calls.
(define value
  (lambda (ex e)
    (cond
      ((null? ex) 0)
      ((math? ex e) (mathexvalue ex e))
      ((predicate?  ex e)        )
;      ((bool? (car ex ) e)              )
      (else 'youfuckedup))))

;returns the evaluated value of a math-only expression. 
(define mathexvalue
  (lambda (ex e)
    (cond
      ((number? ex) ex)
      ((null? (cdr ex)) (mathexvalue (car ex) e))
      ((not(math? ex e)) 'notexpression)
      (else (cond
              ((eq? '= (operator ex)) (assign-stmt ex e))
              ((eq? '+ (operator ex)) (+ (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '- (operator ex)) (- (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '/ (operator ex)) (quotient (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '* (operator ex)) (* (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '% (operator ex)) (remainder (mathexvalue (operand1 ex) e) (mathexvalue(operand2 ex) e))))))))
(define predvalue
  (lambda (ex e)
    (cond
      ((number? ex) ex)
      ((null? (cdr ex)) predvalue (car ex) e))
      ((not(predicate? ex e)) (value ex e))
      (else (cond
              ((eq? '== (operator ex)) (eq? (predvalue (operand1 ex) e)(predvalue (operand2 ex) e)))
              ((eq? '!= (operator ex)) (not(eq? (predvalue (operand1 ex) e)(predvalue (operand2 ex) e))))
              ((eq? '> (operator ex)) (> (predvalue (operand1 ex) e) (predvalue (operand2 ex) e)))
              ((eq? '< (operator ex)) (< (predvalue (operand1 ex) e) (predvalue (operand2 ex) e)))
              ((eq? '<= (operator ex)) (<= (predvalue (operand1 ex) e) (predvalue (operand2 ex) e)))
              ((eq? '>= (operator ex)) (>= (predvalue (operand1 ex) e) (predvalue(operand2 ex) e)))))))



; ashit function that I made so that my other functions have something to call.
;(define lookup
 ; (lambda (ex e)e))