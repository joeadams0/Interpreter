(load "classParser.scm")
(load "environment.scm")
(load "Interpreter_value.scm")

; Begins interpretation process
(define interpret
  (lambda (filename class-name)
    (func-call 'main '() class-name '() (class-list (parser filename) (new-environment)))))

; Parses the class list
; Returns an environment with all of th classes in it
(define class-list
  (lambda (l e)
    (cond
      ((null? l) e)
      (else (class-list (cdr l) (class-declare (car l) e))))))

; Interpretes the statement list
; p is a parsetree, e is the environment
; returns an environment
(define stmt-list
  (lambda (p e return break continue class instance)
    (cond
      ((null? p) (end-block e))
      (else (stmt-list (cdr p) (stmt (car p) e return break continue class instance) return break continue class instance)))))

; Declares a new class
; Returns an environment
(define class-declare
  (lambda (class e)
    (bind-class (car (cdr class)) (add-fields (car (cdr (cdr (cdr class)))) (set-parent (car (cdr (cdr class))) (new-class)) e) e)))

; Parses the class fields
; Returns a class
(define add-fields
  (lambda (l class e)
    (cond
      ((null? l) class)
      (else (add-fields (cdr l) (new-field (car l) class e) e)))))

; Adds a new field to class
; Returns the class
(define new-field
  (lambda (field class e)
    (cond
      ((eq? (operator field) 'static-var) (static-var-dec field class e))
      ((eq? (operator field) 'static-function) (method-dec field class e))
      ((eq? (operator field) 'var) (instance-var-dec field class e))
      ((eq? (operator field) 'function) (method-dec field class e)))))

; Adds a static var
; returns the class
(define static-var-dec  
  (lambda (field class e)
    (cond
      ((not (null? (lookup-var (car (cdr field)) class '() e #f))) (error 'static-var-dec "Variable already exists"))
      ((= (length field) 2) (bind-static-var (car (cdr field)) '(1) class))
      (else (bind-static-var (car (cdr field)) (value (car (cdr (cdr field))) e class '()) class)))))

; Adds an instance variable
; Returns the class
(define instance-var-dec
  (lambda (field class e)
    (cond
      ((not (null? (lookup-var (car (cdr field)) class '() e #f))) (error 'static-var-dec "Variable already exists"))
      (else (bind-instance-var (car (cdr field)) class)))))

; (params, body, class function)
; Returns environemnt
(define method-dec
  (lambda (field class e)
    (cond
      ((not (null? (lookup-method (car (cdr field)) class))) (error 'func-declare "Method already exists"))
      ((bind-method (car (cdr field)) (cons (car (cdr (cdr field))) (cons (car (cdr (cdr (cdr field)))) (lambda (e) (lookup-class (car (cdr (field))) e)))) class)))))  
    
; Interpretes a statement
; s is the statement, e is the environment
; returns an environment
(define stmt
  (lambda (s e return break continue class instance)
    (cond
      ((eq? (operator s) '=) (assign-stmt s e class instance) e)
      ((eq? (operator s) 'var) (define-stmt s e class instance))
      ((eq? (operator s) 'break) (break e))
      ((eq? (operator s) 'if) (if-stmt s e return break continue  class instance))
      ((eq? (operator s) 'begin) (start-block s e return break continue  class instance))
      ((eq? (operator s) 'continue) (continue e))
      ((eq? (operator s) 'function) (function-declare s e class instance))
      ((eq? (car s) 'funcall) (method-call s e  class instance) e)
      (else ((stmt-f s) s e return  class instance)))))

; Interpretes the if statement
; s is the statement, e is the environment 
; returns an environment
(define if-stmt
  (lambda (s e return break continue )
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
               

(define method-call
  (lambda (s e class instance)
    (func-call (car (cdr s)) (cdr (cdr s)) class instance e)))

(define func-call
  (lambda (f-name params class-name instance e)
    (cond
      ((null? (lookup-method f-name  (lookup-class class-name e))) (error 'func-call "Function not declared before use"))
      (else (call/cc (lambda (return)
                       (stmt-list (get-method-body f-name (lookup-class class-name e)) (func-params (get-method-params f-name (lookup-var class-name e)) params (push-layer (get-base-env e)) (lookup-class class-name e) instance) return '() '() (lookup-class class-name e) instance))))))) 


(define get-method-body
  (lambda (m-name class)
    (car (cdr (lookup-method m-name class)))))

(define get-method-params
  (lambda (m-name class)
    (car (lookup-method m-name class))))

(define func-params 
  (lambda (vars params e class instance)
    (cond
      ((and (null? vars) (null? params)) e)
      ((or (null? vars) (null? params)) (error 'func-params "Function input does not match parameters required"))
      ((eq? (car vars) '&) (func-params (cdr (cdr vars)) (cdr params) (bind-pointer (car (cdr vars)) (lookup-var (car params) class instance e #t) e)))
      (else (func-params (cdr vars) (cdr params) (bind (car vars) (value (car params) e) e))))))

                              
; Interpretes the define a variable statement
; s is the statement, e is the environment
; returns an environment
(define define-stmt
  (lambda (s e class instance)
    (cond
      ((not (null? (lookup-var (operand1 s) class instance e #t))) (error 'define-stmt "Variable is already defined"))
      ; Just declare statement
      ((null? (operand2 s)) (bind (operand1 s) '(1) e))
      ; declare and assign
      (else (bind (operand1 s) (value (operand2 s) e class instance) e)))))

; Interpretes the variable assignment statement
; s is the statement, e is the environment
; returns a list which is (value environment)
(define assign-stmt
  (lambda (s e class instance)
    (cond
      ((null? (lookup-var (operand1 s) class instance e #f)) (error 'assign-stmt "Variable not declared"))
      ; Return the value consed onto a list containing the environment
      (else (update-binding (operand1 s) (value (operand2 s) e class instance) e class instance))))) 

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
  (lambda(s e return class instance)
      (return (value (car (cdr s)) e class instance))))

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


; Evaluates the dot expression
; Return the class that the statment evaluates to
; x.y = 10 (dot-eval x
(define left-dot
  (lambda (s e class instance)
    1))
