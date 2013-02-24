;OPERATOR/OPERAND PARSING FUNCTIONS==============================
;FORMAT: (operator operand1 operand2)
; moved to interpreter.rkt
;END OPERATOR/OPERAND PARSING FUNCTIONS==============================

;EXPRESSION CHECKING FUNCTIONS===============================
;checks if the list passed in is any kind of expression, math, predicate, or boolean.
(define expression?
  (lambda (ex e)
    (cond
      ((math? ex e) #t)
      ((predicate? ex e) #t)
      ((bool? ex e) #t)
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
      ((not (= (length ex) 3)) #f)
      ((or (eq? '! (operator ex)) (eq? '&& (operator ex)) (eq? '|| (operator ex)))(and (bool? (operand1 ex)) (bool? (operand2 ex))))
      (else #f))))
;END EXPRESSION CHECKING FUNCTIONS===============================

;THE EVALUATOR FUNCTIONS=========================================
;the main thing the rest of the interpreter calls. It evaluates valid math, predicate and boolean functions. if you pass in something else, it hates you
;it takes the expression (which can contian nested expressions) and an environment
;it returns a (value environment) pair, or an error message
(define value
  (lambda (ex e)
    (cond
      ((null? ex) 'youfuckedup)
      ((math? ex e) (mathexvalue ex e))
      ((predicate? ex e) (predvalue ex e))
      ((bool? (car ex ) e) (bval ex e))
      (else 'youfuckedup))))
;returns the evaluated value of a math-only expression. 
(define mathexvalue
  (lambda (ex e)
    (cond
      ((number? ex) (teamup ex e)) ;is the expression a number? return the number
      ((not(null? (lookup ex e))) (teamup(lookup ex e) e)) ;is the expression a valid variable? return the value of the variable
      ((null? (cdr ex)) (mathexvalue (car ex) e))
      ((not(math? ex e)) 'notexpression)
      (else (cond
              ((eq? '= (operator ex)) (assign-stmt ex e))
              ((eq? '+ (operator ex)) (handleapply + (mathexvalue (operand1 ex) e) (operand2 ex)))
              ((eq? '- (operator ex)) (handleapply - (mathexvalue (operand1 ex) e) (operand2 ex)))
              ((eq? '/ (operator ex)) (handleapply quotient (mathexvalue (operand1 ex) e) (operand2 ex)))
              ((eq? '* (operator ex)) (handleapply * (mathexvalue (operand1 ex) e) (operand2 ex)))
              ((eq? '% (operator ex)) (handleapply remainder (mathexvalue (operand1 ex) e) (operand2 ex))))))))
(define predvalue
  (lambda (ex e)
    (cond
      ((number? ex) (teamup ex e))
      ((not(null? (lookup ex e))) (teamup(lookup ex e)e))
      ((null? (cdr ex)) (predvalue (car ex) e))
      ((not(predicate? ex e)) (value ex e))
      (else (cond
              ((eq? '== (operator ex)) (handleapply eq? (predvalue (operand1 ex) e)(operand2 ex)))
              ((eq? '!= (operator ex)) (handleapply (lambda (a b) (not(eq? a b))) (predvalue (operand1 ex)e) (operand2 ex)))
              ((eq? '> (operator ex)) (handleapply > (predvalue (operand1 ex) e)(operand2 ex)))
              ((eq? '< (operator ex)) (handleapply < (predvalue (operand1 ex) e)(operand2 ex)))
              ((eq? '<= (operator ex)) (handleapply <= (predvalue (operand1 ex) e)(operand2 ex)))
              ((eq? '>= (operator ex)) (handleapply >= (predvalue (operand1 ex) e)(operand2 ex))))))))
(define bval
  (lambda (ex e)
    (cond
      ((number? ex) ex)
      ((not(null? (lookup ex e))) (lookup ex e))
      ((null? (cdr ex)) (bval (car ex) e))
      ((not(bool? ex e)) (value ex e))
      (else (cond
              ((eq? '! (operator ex)) (handleapply not (bval (operand1 ex) e)(operand2 ex)))
              ((eq? '&& (operator ex)) (handleapply and (predvalue (operand1 ex)e) (operand2 ex)))
              ((eq? '|| (operator ex)) (handleapply or (bval (operand1 ex) e)(operand2 ex))))))))
;END EVALUATOR FUNCTIONS=========================================

;HELPER FUNCTIONS================================================
(define teamup
  (lambda (val e)
    (cons val (cons e '()))))
;takes in a (value envirmonment) pair as lhs and an expression as rhs
;then it applies the f function to the value of lhs and the value of the evaluated rhs
(define handleapply 
  (lambda (f lhs rhs)
    (teamup (f (get-val lhs) (get-val (value rhs (get-env lhs)))) (get-env (value rhs (get-env lhs))))))