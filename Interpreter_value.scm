;OPERATOR/OPERAND PARSING FUNCTIONS==============================
;FORMAT: (operator operand1 operand2)
; moved to interpreter.rkt
;END OPERATOR/OPERAND PARSING FUNCTIONS==============================

;EXPRESSION CHECKING FUNCTIONS===============================
;checks if the list passed in is any kind of expression, math, predicate, or boolean.
(define expression?
  (lambda (ex e)
    (cond
      ((bool? ex e) #t)
      ((math? ex e) #t)
      ((predicate? ex e) #t)
      (else #f))))
;returns #t if the list is a valid math expression, #f otherwise
(define math?
  (lambda (ex e)
    (cond
      ((null? ex) #f)
      ((number? ex) #t)
      ((not (pair? ex)) (or(number? (lookup ex e))(or (eq? (lookup ex e) #t)(eq? (lookup ex e) #f))))
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
      ((number? ex) #t)
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
      ((not (= (length ex) 3)) (if
                                (and (= (length ex) 2) (eq? '! (operator ex))) (or(bool? (operand1 ex)e)(predicate? (operand1 ex) e))
                                #f))
      ((or (eq? '&& (operator ex)) (eq? '|| (operator ex)))(and (bool? (operand1 ex) e) (bool? (operand2 ex) e)))
      (else #f))))
;END EXPRESSION CHECKING FUNCTIONS===============================

;THE EVALUATOR FUNCTIONS=========================================
;the main thing the rest of the interpreter calls. It evaluates valid math, predicate and boolean functions. if you pass in something else, it hates you
;it takes the expression (which can contian nested expressions) and an environment
;it returns a (value environment) pair, or an error message
(define value
  (lambda (ex e)
    (cond
      ((null? ex) (error 'value "Invalid expression"))
      ((bool? ex e) (bval ex e))
      ((math? ex e) (mathexvalue ex e))
      ((predicate? ex e) (predvalue ex e))
      (else (error 'value "Invalid expression")))))
;returns the evaluated value of a math-only expression. 
(define mathexvalue
  (lambda (ex e)
    (cond
      ((number? ex) ex) ;is the expression a number? return the number
      ((not(null? (lookup ex e))) (lookup ex e)) ;is the expression a valid variable? return the value of the variable
      ((null? (cdr ex)) (mathexvalue (car ex) e))
      ((not(math? ex e)) 'notexpression)
      (else (cond
              ((eq? '= (operator ex)) (assign-stmt ex e))
              ((eq? '+ (operator ex)) (+(mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '- (operator ex)) (- (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '/ (operator ex)) (quotient (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '* (operator ex)) (* (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e)))
              ((eq? '% (operator ex)) (remainder (mathexvalue (operand1 ex) e) (mathexvalue (operand2 ex) e))))))))
(define predvalue
  (lambda (ex e)
    (cond
      ((number? ex) ex)
      ((not(null? (lookup ex e))) (lookup ex e))
      ((null? (cdr ex)) (predvalue (car ex) e))
      ((not(predicate? ex e)) (value ex e)) 
      (else (cond
              ((eq? '== (operator ex)) (eq? (value (operand1 ex) e)(value (operand2 ex) e)))
              ((eq? '!= (operator ex)) (not (eq? (predvalue (operand1 ex) e) (predvalue (operand2 ex) e))))
              ((eq? '> (operator ex)) (> (value (operand1 ex) e)(value (operand2 ex) e)))
              ((eq? '< (operator ex)) (< (value (operand1 ex) e)(value (operand2 ex) e)))
              ((eq? '<= (operator ex)) (<= (value (operand1 ex) e) (value (operand2 ex) e)))
              ((eq? '>= (operator ex)) (>= (value (operand1 ex) e)(value (operand2 ex) e))))))))
(define bval
  (lambda (ex e)
    (cond
      ((not(null? (lookup ex e))) (lookup ex e))
      ((null? (cdr ex)) (bval (car ex) e))
      ((not(bool? ex e)) (value ex e))
      (else (cond
              ((eq? '! (operator ex)) (not (value (operand1 ex) e)))
              ((eq? '&& (operator ex)) ((lambda (a b) (and a b)) (bval (operand1 ex)e) (bval (operand2 ex)e)))
              ((eq? '|| (operator ex)) ((lambda (a b) (or a b)) (bval (operand1 ex) e) (bval (operand2 ex)e))))))))
;END EVALUATOR FUNCTIONS=========================================