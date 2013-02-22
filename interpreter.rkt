(load "verySimpleParser.scm")
(load "environment.rkt")
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
    e))
      

; Interpretes the define a variable statement
; s is the statement, e is the environment
; returns an environment
(define define-stmt
  (lambda (s e)
    (cond
      ((null? (second-operand s)) (set-var(first-operand s) '1 e))
      (else (set-var (first-operand s) (value (second-operand s) e) e)))))

; Interpretes the variable assignment statement
; s is the statement, e is the environment
; returns a list which is (value environment)
(define assign-stmt
  (lambda (s e)
    (cond
      ((null?(lookup (first-operand s) e)) (error 'interpreter "Variable not declared"))
      (else (set-var (first-operand s) (value (second-operand s) e) e)))))

; Interpretes the return statement
; s is the statement, e is the environment
; returns an environment
(define return-stmt
  (lambda(s e)
    (set-var 'return (value (car (cdr s)) e) e)))

; Returns the value of the statement, predicate or otherwise 
; s is the statement, e is the environment
; returns a list which is: (value environmet)
(define value
  (lambda (s e)
    s))

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

; Generates stmt-type? checkers for a given stmt
; Returns a function that can be called to check the type of the statement passed in
(define stmt-type?-gen
  (lambda (s)
    (lambda (type)
      (eq? type (operator s)))))

; Sets the variable and removes the old one
; returns an environment
(define set-var
  (lambda (var value e)
    (bind var value (remove-binding var e))))

; Takes in a list that is (value environment) and a function, then calls the function, f, like so: (f value environment)
; Returns whatever f returns
(define side-effect-helper
  (lambda (l f)
    (f (car l) (car (cdr l)))))

; Gets the first operand 
; Returns the operand
(define first-operand
  (lambda (s)
    (cond 
      ((null? (cdr s)) '())
      (else (car (cdr s))))))

; Gets the second operand 
; Returns the operand
(define second-operand
  (lambda (s)
    (cond
      ((or (null? (cdr s)) (null? (cdr (cdr s)))) '())
      (else (car (cdr (cdr s)))))))

; Gets the operator
; Returns the operator
(define operator
  (lambda (s)
    (cond
      ((null? s) '())
      (else (car s)))))

