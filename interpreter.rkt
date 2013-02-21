(load "verySimpleParser.scm")
(load "environment.rkt")
(load "Interpreter_value.rkt")
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
      ((null? (cdr (cdr s))) (set-var(car (cdr s)) '1 e))
      (else (set-var (car (cdr s)) (value (car (cdr (cdr s))) e) e)))))

; Interpretes the variable assignment statement
; s is the statement, e is the environment
; returns a value, updates the environment
(define assign-stmt
  (lambda (s e)
    e))

; Interpretes the return statement
; s is the statement, e is the environment
; returns an environment
(define return-stmt
  (lambda(s e)
    (set-var 'return (value (car (cdr s)) e) e)))

; Returns the value of the statement, predicate or otherwise 
; s is the statement, e is the environment
; returns an int or a boolean


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
      (eq? type (car s)))))

; Sets the variable and removes the old one
; returns an environment
(define set-var
  (lambda (var value e)
    (remove-binding var e)
    (bind var value e)))
