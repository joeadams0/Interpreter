(load "verySimpleParser.scm")
; Begins interpretation process
(define interprete
  (lambda (filename)
    filename))

; Interpretes the statement list
; p is a parsetree, e is the environment
; returns an environment
(define stmt-list
  (lambda (p e)
    e))

; Interpretes a statement
; s is the statement, e is the environment
; returns an environment
(define stmt
  (lambda (s e)
    e))

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
    e))

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
    e))

; Returns the value of the statement, predicate or otherwise 
; s is the statement, e is the environment
; returns an int or a boolean
(define value
  (lambda (s e)
    e))

