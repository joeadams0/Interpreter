(load "functionParser.scm")
(load "environment.scm")
(load "Interpreter_value.scm")

; Begins interpretation process
(define interpret
  (lambda (filename)
    (letrec ((env (call/cc (lambda (return)
               (stmt-list (parser filename) (new-environment) return '() '())))))
      (cond
        ((not (null? (lookup 'main env)))
         (display-filter (func-call (cons 'funcall (cons 'main '())) env)))
        (else (display "Warning: No main method defined. Program has to entry point"))))))

; Interpretes the statement list
; p is a parsetree, e is the environment
; returns an environment
(define stmt-list
  (lambda (p e return break continue)
    (cond
      ((null? p) (end-block e))
      (else (stmt-list (cdr p) (stmt (car p) e return break continue) return break continue)))))

; Interpretes a statement
; s is the statement, e is the environment
; returns an environment
(define stmt
  (lambda (s e return break continue)
    (cond
      ((eq? (operator s) '=) (assign-stmt s e) e)
      ((eq? (operator s) 'var) (define-stmt s e))
      ((eq? (operator s) 'break) (break e))
      ((eq? (operator s) 'if) (if-stmt s e return break continue))
      ((eq? (operator s) 'begin) (start-block s e return break continue))
      ((eq? (operator s) 'continue) (continue e))
      ((eq? (operator s) 'function) (function-declare s e))
      ((eq? (car s) 'funcall) (func-call s e) e)
      (else ((stmt-f s) s e return)))))

; Interpretes the if statement
; s is the statement, e is the environment 
; returns an environment
(define if-stmt
  (lambda (s e return break continue)
    (cond
      ; If it is an if statement 
      ((eq? (operator s) 'if) (if-eval (value (operand1 s) e) (operand2 s) (operand3 s) e return break continue))
      ; else statement
      (stmt s e))))

; Interpretes the while statements
(define while-stmt
  (lambda (s e return)
    (call/cc (lambda (break) 
               (letrec ((loop (lambda (cond body env)
                                (if (value cond env)    
                                    (loop cond body (stmt body e return break (lambda (e2) 
                                                                                (loop cond body e2))))
                                    (break env)))))
                 (loop (operand1 s) (operand2 s) e))))))


(define function-declare
  (lambda (s e)
    (cond
      ((not (null? (lookup (car (cdr s)) e))) (error 'function-declare "Function name already in use"))
      (else (bind (car (cdr s)) (cdr (cdr s)) e)))))
               

(define func-call
  (lambda (s e)
    (cond
      ((null? (lookup (operand1 s) e)) (error 'func-call "Function not declared before use"))
      (else (call/cc (lambda (return)
                       (stmt-list (car (cdr (lookup (operand1 s) e))) (func-params (car (lookup (operand1 s) e)) (cdr (cdr s)) (push-layer e)) return '() '()))))))) 

      
(define func-params 
  (lambda (vars params e)
    (cond
      ((and (null? vars) (null? params)) e)
      ((or (null? vars) (null? params)) (error 'func-params "Function input does not match parameters required"))
      ((eq? (car vars) '&) (func-params (cdr (cdr vars)) (cdr params) (bind-pointer (car (cdr vars)) (lookup-pointer (car params) e) e)))
      (else (func-params (cdr vars) (cdr params) (bind (car vars) (value (car params) e) e))))))

                              
; Interpretes the define a variable statement
; s is the statement, e is the environment
; returns an environment
(define define-stmt
  (lambda (s e)
    (cond
      ((not (null? (lookup (operand1 s) e))) (error 'define-stmt "Variable is already defined"))
      ; Just declare statement
      ((null? (operand2 s)) (bind (operand1 s) '(1) e))
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

; Adds another layer to the environment
; Starts statement list on that block
(define start-block
  (lambda (s e return break continue)
    (stmt-list (cdr s) (push-layer e) return (lambda (e) (break (end-block e))) (lambda (e2) (continue (pop-layer e2)))))) 

(define end-block
  (lambda (e)
    (cond
      ((null? (pop-layer e)) e)
      (else (pop-layer e)))))

; Interpretes the return statement
; s is the statement, e is the environment
; returns an environment
(define return-stmt
  (lambda(s e return)
      (return (value (car (cdr s)) e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Determines the function that is needed to interprete the statement type
; returns a function
(define stmt-f
  (lambda (s)
    (define stmt-type? (stmt-type?-gen s))
    (cond
      ((stmt-type? 'return) return-stmt)
      ((stmt-type? 'while) while-stmt)
      (else value))))

; Evaluates the if statement
; returns an environment
(define if-eval
  (lambda (bool then else e return break continue)
    (cond
      ; Predicate evaluates to true, do the then stmt
      (bool (stmt then e return break continue))
      ; If there isn't another if or else stmt, then return the environment
      ((null? else) e)
      ; Do the else stmt
      (else (stmt else e return break continue)))))

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

(define display-filter
  (lambda (in)
    (cond
      ((boolean? in)
       (if in
           'true
           'false))
      ((list? in) (error 'display-filter "Something is wrong here... trying to return list."))
      (else in))))

