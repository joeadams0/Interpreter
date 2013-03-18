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
      ((eq? (operator s) 'if) (if-wrapper (value (operand1 s) e) (operand2 s) (operand3 s)))
      ; else statement
      (stmt s e))))
      
                              
; Interpretes the define a variable statement
; s is the statement, e is the environment
; returns an environment
(define define-stmt
  (lambda (s e)
    (cond
      ; Just declare statement
      ((null? (operand2 s)) (set-var (operand1 s) '0 e))
      ; declare and assign
      (else (set-var-wrapper (operand1 s) (value (operand2 s) e))))))

; Interpretes the variable assignment statement
; s is the statement, e is the environment
; returns a list which is (value environment)
(define assign-stmt
  (lambda (s e) 
    (cond
      ((null?(lookup (operand1 s) e)) (error 'interpreter "Variable not declared"))
      ; Return the value consed onto a list containing the environment
      (else (assign-wrapper (operand1 s) (value (operand2 s) e)))))) 

; Interpretes the return statement
; s is the statement, e is the environment
; returns an environment
(define return-stmt
  (lambda(s e)
    (set-var-wrapper 'return (value (car (cdr s)) e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Determines the function that is needed to interprete the statement type
; returns a function
(define stmt-f
  (lambda (s)
    (define stmt-type? (stmt-type?-gen s))
    (cond
      ((stmt-type? '=) assign-stmt-env)
      ((stmt-type? 'if) if-stmt)
      ((stmt-type? 'var) define-stmt)
      ((stmt-type? 'return) return-stmt)
      (else value))))

; Evaluates the if statement
; returns an environment
(define if-eval
  (lambda (predicate then else e)
    (cond
      ; Predicate evaluates to true, do the then stmt
      (predicate (stmt then e))
      ; If there isn't another if or else stmt, then return the environment
      ((null? else) e)
      ; Do the else stmt
      ((stmt else e)))))

; Generates stmt-type? checkers for a given stmt
; Returns a function that can be called to check the type of the statement passed in
(define stmt-type?-gen
  (lambda (s)
    (lambda (type)
      (eq? type (operator s)))))

; Sets the variable and removes the old one
; returns a list: (value newenvironment)
(define set-var
  (lambda (var value e)
    (bind var value (remove-binding var e))))

; Takes in a list that is (value environment) and a function, then calls the function, f, like so: (f value environment)
; Returns whatever f returns
(define side-effect-f
  (lambda (f l)
    (f (car l) (car (cdr l)))))

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

; Returns the environment from a list: (value environment)
(define get-env
  (lambda (l)
    (cond 
      ((or (null? l) (null? (cdr l))) (error 'get-env "Bad List"))
      (else (car (cdr l))))))

; Returns the value from a list: (value environment)
(define get-val
  (lambda (l)
    (cond 
      ((not (pair? l)) (error 'get-val "Bad List"))
      (else (car l)))))

; Returns environment only from the assignment stmt
(define assign-stmt-env
  (lambda (s e)
    (get-env (assign-stmt s e))))

; Evaluates the if stmt on the value and the environment passed in as a list: (value environment)
; Returns an environment
(define if-wrapper
  (lambda (l then else)
    (if-eval (get-val l) then else (get-env l))))

; Wrappes the set-var function. var is the variable to set and l is the (value environment) pair
(define set-var-wrapper
  (lambda (var l)
    (set-var var (get-val l) (get-env l))))

; Wraps the assignment stmt for sideeffects
(define assign-wrapper
  (lambda (var val)
    (teamup (get-val val) (set-var-wrapper var val))))

(define teamup
  (lambda (val e)
    (cons val (cons e '()))))