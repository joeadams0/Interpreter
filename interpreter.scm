(load "loopSimpleParser.scm")
(load "environment.scm")
(load "Interpreter_value.scm")

; Begins interpretation process
(define interpret
  (lambda (filename)
    (lookup 'return (stmt-list (parser filename) (new-environment)))))

; Interpretes the statement list
; p is a parsetree, e is the environment
; returns an environment
(define stmt-list
  (lambda (p e)
    (cond
      ((null? p) e)
      ((eq? (operator (car p)) '=) (stmt (car p) e) (stmt-list (cdr p) e))
      (else (stmt-list (cdr p) (stmt (car p) e))))))

; Interpretes a statement
; s is the statement, e is the environment
; returns an environment
(define stmt
  (lambda (s e)
    ((stmt-f s) s e)))

; Interprete the if statement
; s is the statement, e is the environment 
; returns an environment
(define if-stmt
  (lambda (s e)
    (cond
      ; If it is an if statement 
      ((eq? (operator s) 'if) (if-eval (value (operand1 s) e) (operand2 s) (operand3 s) e))
      ; else statement
      (stmt s e))))
      
                              
; Interpretes the define a variable statement
; s is the statement, e is the environment
; returns an environment
(define define-stmt
  (lambda (s e)
    (cond
      ((not (null? (lookup (operand1 s) e))) (error 'define-stmt "Variable is already defined"))
      ; Just declare statement
      ((null? (operand2 s)) (bind(operand1 s) '(1) e))
      ; declare and assign
      (else (bind (operand1 s) (value (operand2 s) e) e)))))

; Interpretes the variable assignment statement
; s is the statement, e is the environment
; returns a list which is (value environment)
(define assign-stmt
  (lambda (s e)
    (cond
      ((null?(lookup (operand1 s) e)) (error 'assign-stmt "Variable not declared"))
      ; Return the value consed onto a list containing the environment
      (else (update-binding (operand1 s) (value (operand2 s) e) e))))) 

; Interpretes the return statement
; s is the statement, e is the environment
; returns an environment
(define return-stmt
  (lambda(s e)
    (bind 'return (value (car (cdr s)) e) e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Determines the function that is needed to interprete the statement type
; returns a function
(define stmt-f
  (lambda (s)
    (define stmt-type? (stmt-type?-gen s))
    (cond
      ((stmt-type? '=) assign-stmt)
      ((stmt-type? 'if) if-stmt)
      ((stmt-type? 'var) define-stmt)
      ((stmt-type? 'return) return-stmt)
      (else value))))

; Evaluates the if statement
; returns an environment
(define if-eval
  (lambda (bool then else e)
    (cond
      ; Predicate evaluates to true, do the then stmt
      (bool (if (eq? (car then) 'begin) 
                (stmt-list then e)
                (stmt-list (cons then '()) e)))
      ; If there isn't another if or else stmt, then return the environment
      ((null? else) e)
      ; Do the else stmt
      (else (if (eq? (car else) 'begin) 
                (stmt-list else e)
                (stmt-list (cons else '()) e))))))

; Generates stmt-type? checkers for a given stmt
; Returns a function that can be called to check the type of the statement passed in
(define stmt-type?-gen
  (lambda (s)
    (lambda (type)
      (eq? type (operator s)))))

; Gets the first operand 
; Returns the operand
(define operand1
  (lambda (s)
    (cond 
      ((null? (cdr s)) '())
      (else (car (cdr s))))))

; Gets the second operand 
; Returns the operand
(define operand2
  (lambda (s)
    (cond
      ((or (null? (cdr s)) (null? (cdr (cdr s)))) '())
      (else (car (cdr (cdr s)))))))

; Gets the third operand 
; Returns the operand
(define operand3
  (lambda (s)
    (cond
      ((or (or (null? (cdr s)) (null? (cdr (cdr s)))) (null? (cdr (cdr (cdr s))))) '())
      (else (car (cdr (cdr (cdr s))))))))

; Gets the operator
; Returns the operator
(define operator
  (lambda (s)
    (cond
      ((null? s) '())
      (else (car s)))))

