(load "classParser.scm")
(load "environment.scm")
(load "Interpreter_value.scm")

; Begins interpretation process
(define interpret
  (lambda (filename class-name)
    (let ((env (class-list (parser filename) (new-environment))))
      (let ((val (func-call 'main '() (lookup-class class-name env) '() env)))
        (if (boolean? val)
            (if val
                'true
                'false)
            val)))))

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
    (bind-class (car (cdr class)) (add-fields (car (cdr (cdr (cdr class)))) (set-parent (get-p class) (new-class)) e) e)))

(define get-p
  (lambda (class)
    (cond
      ((null? (car (cdr (cdr class)))) '())
      (else (car (cdr (car (cdr (cdr class)))))))))

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
      ((and (list? (car (cdr (cdr field)))) (eq? 'new (operator (car (cdr (cdr field)))))) 
                   (bind-instance-variable (car (cdr field)) (new-instance (car (cdr (cdr field)))) class #f))
      (else (bind-instance-variable (car (cdr field)) (value (car (cdr (cdr field))) e class '()) class)))))

; (params, body, class function)
; Returns environemnt
(define method-dec
  (lambda (field class e)
    (cond
      ((not (null? (lookup-method (car (cdr field)) class e))) (error 'func-declare "Method already exists"))
      ((bind-method (car (cdr field)) 
                    (cons (car (cdr (cdr field))) 
                          (cons (car (cdr (cdr (cdr field)))) 
                                (list (lambda (e) (lookup-class (car (cdr (field))) e)))))
                    class)))))
    
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
               

; Takes a method stmt and parses it up to call the method on the parameters
; Returns a value
(define method-call
  (lambda (s e class instance)
    (cond
      ((dot? (car (cdr s))) 
       (let ((dot (dot-eval (car (cdr s)) e class instance)))
         (func-call (car (cdr dot)) (cdr (cdr s)) (car (car dot)) (car (cdr (car dot))) e)))
      (else (func-call (car (cdr s)) (cdr (cdr s)) class instance e)))))

; Performs a function call
; Returns a value
(define func-call
  (lambda (f-name params class instance e)
    (cond
      ((null? class) (error f-name "Function not declared before use"))
      ((null? (lookup-method f-name  class e)) (func-call f-name params (lookup-class (lookup-parent class instance) e) instance e))
      (else (call/cc (lambda (return)
                       (let ((m-closure (lookup-method f-name class e)))
                         (stmt-list (get-method-body m-closure) (func-params (get-params m-closure) params (push-layer (get-base-env e)) e class instance) return '() '() class instance)))))))) 

; Gets the method body corrisponding to the name passed in in a certain class
; Returns the method closure
(define get-method-body
  (lambda (m-closure)
    (car (cdr m-closure))))

; Gets the method parameters from a method closure
; Returns the list of parameters
(define get-params
  (lambda (m-closure)
    (car m-closure)))

; Takes the formal parameters and the values and returns the new environment 
(define func-params 
  (lambda (vars params e  oldenv class instance)
    (cond
      ((and (null? vars) (null? params)) e)
      ((or (null? vars) (null? params)) (error 'func-params "Function input does not match parameters required"))
      ((eq? (operator vars) '&) 
       (if (and (list? (car params)) (eq? (operator (car params)) 'dot))
           (func-params (cdr (cdr vars)) (cdr params) (bind-pointer (car (cdr vars)) (dot-var-lookup (car params) oldenv class instance #t) e) oldenv class instance)
           (func-params (cdr (cdr vars)) (cdr params) (bind-pointer (car (cdr vars)) (lookup-var (car params) class instance oldenv #t) e) oldenv class instance)))
      (else 
       (func-params 
        (cdr vars) 
        (cdr params) 
        (bind (car vars) (value (car params) oldenv class instance) e) 
        oldenv class instance)))))

                              
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
      ((and (list? (operand1 s)) (eq? 'dot (car (operand1 s))))
       (if (null? (dot-var-lookup (operand1 s) e class instance #t))
           (error 'assign-stmt "Variable not declared")
           (set-box! (dot-var-lookup (operand1 s) e class instance #t) (value (operand2 s) e class instance))))
      (else
       (if (null? (lookup-var (operand1 s) class instance e #f)) 
           (error 'assign-stmt "Variable not declared")
           ; Return the value consed onto a list containing the environment
           (set-box! (lookup-var (operand1 s) class instance e #t) (value (operand2 s) e class instance)))))))

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


(define dot?
  (lambda (s)
    (cond
      ((not (list? s)) #f)
      ((eq? 'dot (car s)) #t)
      (else #f))))

; Evaluates the dot expression
; Returns (class instance rhs)
; x.y = 10 (dot-eval x
(define dot-eval
  (lambda (s e class instance)
    (cons (lhs-eval (car (cdr s)) e class instance) (list (car (cdr (cdr s)))))))

; Returns (class instance)
(define lhs-eval
  (lambda (s e class instance)
    (cond
      ((not (list? s)) 
       (cond
         ((eq? 'this s) (cons class (list instance)))
         ((eq? 'super s) (cons (lookup-parent class instance) (list instance)))
         (else (cons class (list (lookup-var s class instance e #f))))))
      ((eq? (operator s) 'new) (new-instance (car (cdr s)) e))
      (else 
       (let ((dot (dot-var-lookup (operand1 s) e class instance #f)))
         (cons (lookup-class (car (car (cdr dot))) e) (list dot)))))))

(define dot-var-lookup
  (lambda (s e class instance ref?)
    (let ((dot (dot-eval s e class instance)))
      (if (null? (lookup-var (car (cdr dot)) (car (car dot)) (car (cdr (car dot))) e ref?)) 
          (error 'dot-var-lookup "Variable does not exist")
          (lookup-var (car (cdr dot)) (car (car dot)) (car (cdr (car dot))) e ref?)))))


    
    
    