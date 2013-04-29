; testing shit
(define cclass
    '(((a b p poop)(#&1 #&2 #&3 #&4))
      ((m1 m2 m3)
       ((m1 shit to do)(m2 shit to do)(m3 shit to do)))(pareniento)()))
(define eenv
  '(((poop c1 c2 c3)
    ((poop-body) (c1-body) (c2-body) (c3-body)))))
(define tenv
  '(((Square Rectangle) ((((x) (#&5)) ((main) ((() ((return x)) . (something procedure)))) (Rectangle) ()) 
                        (((width) (#&10)) (() ()) (()) ())))))

; The environment for the interpreter
; Return a new environment
; ((classes-name class1) ((classes) (class2)))
(define new-environment
  (lambda ()
    '(
      (()
      ()))))

;(define new-environment
;  (lambda ()
;    (bind 'FALSE #f (bind 'TRUE #t (bind 'false #f (bind 'true #t (push-layer '())))))))


 
(define new-class
  (lambda ()
    '(
      (()())
      (()())
      ()
      (()())  
      ()
      )))

(define new-instance
  (lambda (class-name e)
    (cons (initialize class-name e) (list (list class-name)))))

(define initialize
  (lambda (class-name e)
    (cond 
      ((not (null? (lookup-parent (lookup-class class-name e) '()))) 
       (initialize-class (lookup-instance-vars class-name e) (initialize (lookup-parent (lookup-class class-name e) '()) e)))
      (else (initialize-class (lookup-instance-vars class-name e) '())))))

(define initialize-class
  (lambda (class-vars instance)
    (cond 
      ((null? (first-var class-vars)) instance)
      ((list? (first-value class-vars #t)) (initialize (rest-layer class-vars) (append instance (list (first-value class-vars #t)))))
      (else (initialize-class (rest-layer class-vars) (append instance (list (box (first-value class-vars #t)))))))))
      
; DANIEL
; Class structure -> ( ((static var names)(static var values))  ((method names)(method closures))  (parent)  (instance variable names))
; Sets the parent of the class
; (set-parent 'parent (()()()())) -> (()()(parent)())
; Returns the new class
; Dont use boxes
(define set-parent
  (lambda (parent class)
    (cons 
     (variables-in-class class)
     (cons 
      (methods-in-class class) 
      (cons (enlist parent) (cdddr class))))))

; enlist takes an item and wraps it in parenthases,
; essentially returning the list containing the itme
(define enlist
  (λ (item)
    (cons item '())))

; Adds a variable name to the instance-variable list
; (set-instance-variable 'poop #f '((()())()()())) -> ((()())()()((poop) (#f)))
; Return new class
; No environment
(define bind-instance-variable 
  (lambda (var-name value class )
    (cons    ; put the first part of the class onto the newly re-created rest of it
     (car class)
     (cons  ; put the second part of the class onto the newly re-created rest of it
      (cadr class)
      (cons  ; put the third part of the class onto the newly edited last part of the class
       (caddr class)  ; the 3rd part
       (cons
        (set-var var-name value (cadddr class) #f)
        (list (cdr (cdddr class))))))))) ; add the variable to the 4th part of class
      
; ------------------------------------------------------------------------------
; BIND-XXX FUNCTIONS

; Adds a method to the class
; (bind-method 'poop (poop-closure) '(()  (()())  ()())) -> (()  ((poop)(poop-closure))  ()())
; Return class
; Dont use boxes
(define bind-method
  (lambda (method-name closure class)
    (cons 
     (variables-in-class class)
     (cons
      (cons
       (cons method-name (caadr class))
       (enlist (cons closure (cadadr class))))
      (cddr class)))))

(define bind-constructor
  (lambda (method-name closure class)
    (append (all-but-last class) (list (append (cadr (cdddr class)) (list closure))))))
    
(define (all-but-last l) (reverse (cdr (reverse l))))
    
; Binds a variable to the static variables list in the class
; (bind-static-var 'poop 10 '((()())()()())) -> (((poop)(#&10))()()())
; Returns the new class
; Box the values of the variables before adding them to the list ((box 10) -> #&10)
(define bind-static-var
  (λ (var-name value class)
    (cons 
     (cons
      (cons var-name (caar class))
      (enlist (cons (box value) (car (cdr (car class))))))
     (cdr class))))


(define bind-class
  (lambda (class-name class env)
    (cons (bind-class-layer class-name class env) '())))

; Binds the class to the environment
; (bind-class 'poop (poop-class) ((()())) -> ((poop)((poop-class)))
; Returns the environment
(define bind-class-layer
  (λ (class-name class env)
    (cons (cons class-name (class-name-list env))
          (list (cons class (class-body-list env))))))
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; LOOKUP-XXX FUNCTIONS

(define set-var
  (lambda (var-name value l box?)
    (cond 
      (box? (cons (cons var-name (car l)) (enlist (cons (box value) (car (cdr l))))))
      (else (cons (cons var-name (car l)) (enlist (cons value (car (cdr l)))))))))

(define lookup-method
  (lambda (method class e)
    (lookup-method-layer method (methodnames-list-in-class class) (methodvals-list-in-class class))))

; Looks up a method closure in a class
; (lookup 'poop (()((poop)(poop-closure))()()))  -> poop-closure
; Returns the closure
(define lookup-method-layer
  (lambda (method method-names method-closure)
    (cond
      ((null? method-names) '())
      ((eq? (car method-names) method) (car method-closure))
      (else (lookup-method-layer method (cdr method-names) (cdr method-closure))))))

; Looks up the parent for the class
; (lookup-parent '((()()) (()()) (poop) ()) '()) -> poop
; (lookup-parent '((()()) (()()) () ()) '()) -> '() (if no parent return '())
; Returns the name of the parent class if there is one.
; currently does nothing with the instance
(define lookup-parent
  (lambda (class instance)
    (cond
      ((null? (parent-of class))'())
      (else (car (parent-of class))))))


(define lookup-class
  (lambda (class env)
    (lookup-class-layer class (class-name-list env) (class-body-list env)))) 

; Looks up the class in the environment
; (lookup-class 'poop '((poop) (poop-class))) -> poop-class
; if no class return '()
(define lookup-class-layer
  (lambda (class class-names class-bodies)
    (cond
      ((null? class-names)'())
      ((eq? (car class-names) class) (car class-bodies))
      (else (lookup-class-layer class
                          (cdr class-names)
                                (cdr class-bodies))))))

; Looks up a variable in a class
; (lookup 'poop (((poop)(#&10))()()()) '()) -> 10
; Returns the UNBOXED value of the variable
; Ignore instance for now
(define lookup-var
  (lambda (var class instance e reference?)
    (cond
      ((not (null? (lookup var e reference?))) (lookup var e reference?))
      ((and (not (null? instance)) (not (null? (lookup-instance-var var (cons (get-instance-variables class e) (list (car instance))) reference?)))) 
       (lookup-instance-var var (cons (get-instance-variables class e) (list (car instance))) reference?))
      (else (lookup-static-var var class instance e reference?)))))

(define lookup-static-var 
  (lambda (var class instance e ref?)
    (cond
      ((null? class) '())
      ((null? (lookup-layer var (car class) ref?)) (lookup-static-var var (lookup-class (lookup-parent class instance) e) instance e ref?))
      (else (lookup-layer var (car class) ref?)))))

(define lookup-instance-var
  (lambda (var instance ref?)
    (cond
      ((null? (car instance)) '())
      ((eq? var (first-var instance)) (first-value instance ref?))
      (else (lookup-instance-var var (rest-layer instance) ref?)))))

(define lookup-instance-vars
  (lambda (class-name e)
    (cadddr (lookup-class class-name e))))

(define get-instance-variables
  (lambda (class e)
    (if (not (null? (lookup-parent class '()))) 
        (get-inst-vars class (get-instance-variables (lookup-class (lookup-parent class '()) e) e))
        (get-inst-vars class '()))))

(define get-inst-vars
  (lambda (class l)
    (append l (car (cadddr class)))))

(define lookup-constructor
 (lambda (class-name params e)
   (match-params (get-constructors (lookup-class class-name e)) params)))

(define get-constructors
 (lambda (class)
   (cadr (cdddr class))))

(define match-params
  (lambda (constructors params)
    (cond
      ((null? constructors) '())
      (else (if (eq? (length (get-params (car constructors))) (length params))
                (car constructors)
                (match-params (cdr constructors) params))))))

; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------

(define get-base-env
  (lambda (e)
    (cond
      ((null? (cdr e)) e)
      (else (get-base-env (cdr e))))))
; Abstracted class accessor functions
; Basic environment component gettors
(define class-name-list
  (λ (env)
    (car (car (get-base-env env)))))
(define class-body-list
  (λ (env)
    (car (cdr (car (get-base-env env))))))

; Basic class component gettors
(define variables-in-class
  (λ (class)
    (car class)))
(define methods-in-class
  (λ (class)
    (cadr class)))
(define parent-of
  (λ (class)
    (caddr class)))
(define instance-vars-in-class
  (λ (class)
    (cadddr class)))

; varname accessor functions
(define varnames-list-in-class
  (λ (class)
    (caar class)))
(define first-varname-in-class
  (λ (class)
    (car (varnames-list-in-class class))))
(define rest-of-varnames-in-class
  (λ (class)
    (cdr (varnames-list-in-class class))))

; varval accessor functions
(define varvals-list-in-class
  (λ (class)
    (cadar class)))
(define first-varval-in-class
  (λ (class)
    (car (varvals-list-in-class class))))
(define rest-of-varvals-in-class
  (λ (class)
    (cdr (varvals-list-in-class class))))

; Methodname accessor functions
(define methodnames-list-in-class
  (λ (class)
    (car (methods-in-class class))))
(define first-methodname-in-class
  (λ (class)
    (car (methodnames-list-in-class class))))
(define rest-of-methodnames-in-class
  (λ (class)
    (cdr (methodnames-list-in-class class))))

; method-closure accessor functions
(define methodvals-list-in-class
  (λ (class)
    (cadadr class)))
(define first-methodval-in-class
  (λ (class)
    (car (methodvals-list-in-class class))))
(define rest-of-methodvals-in-class
  (λ (class)
    (cdr (methodvals-list-in-class class))))
; ------------------------------------------------------------------------------


; Bind the variable to the value in the environment
; Returns the new environment
(define bind
  (lambda (var value environment)
    (cons (cons (cons var (get-vars (peek-layer environment))) 
                (cons (cons (box value) (get-vals (peek-layer environment))) '())) 
          (pop-layer environment))))

(define bind-pointer
  (lambda (var value environment)
    (cons (cons 
           (cons var (get-vars (peek-layer environment))) 
           (cons 
            (cons value (get-vals (peek-layer environment))) 
            '())  )
          (pop-layer environment))))


(define update-binding
  (lambda (var value e class instance)
    (let ((ref (lookup-var var (new-class) instance (new-environment) #t)))
      (set-box! ref value))))

(define update-static-var 
  (lambda (var value class)
      (update-value var value (car class))))

(define update-value 
  (lambda (var value l)
    (cond
      ((null? (car l)) (error 'update-value "Variable not declared"))
      ((eq? var (car (car l))) (set-box! (car (car (cdr l))) value))
      (else (update-value var value (cons (cdr (car l)) (list (cdr (car (cdr l))))))))))

                                                                                    
;Updates the value of the binding
; Returns the value
(define update-binding-env
  (lambda (var value e)
    (cond
      ((null? e) (error 'update-binding "Variable not declared"))
      ((null? (car (peek-layer e))) (update-binding-env var value (pop-layer e)))
      ((eq? var (first-var (peek-layer e))) (set-box! (first-value-box (peek-layer e)) value) value)
      (else (update-binding-env var value (cons (rest-layer (peek-layer e)) (pop-layer e)))))))
       
; Removes the binding from the first layer
; Returns an environment
(define remove-binding
  (lambda (var environment)
    (cons (remove-layer-binding var (peek-layer environment) (lambda (v) v)) (pop-layer environment))))

; Removes the binding from the layer if there is one
; Returns modified top layer
(define remove-layer-binding
  (lambda (var layer k)
    (cond
      ((not (pair? (car layer))) (k '(()())))
      ((eq? var (first-var layer)) (remove-layer-binding var (rest-layer layer) k))
      (else (remove-layer-binding var (rest-layer layer) (lambda (layer2)
                                                           (k (cons (cons (first-var layer) (get-vars layer2)) (cons (cons (box (first-value layer #f)) (get-vals layer2)) '())))))))))

; Looks up binding in the environment
; Returns the value bound to the variable
(define lookup
  (lambda (var e ref?)
    (cond
      ((null? e) '())
      ((exists-binding? var (peek-layer e)) (lookup-layer var (peek-layer e) ref?))
      (else (lookup var (pop-layer e) ref?)))))

; Looks up binding in the layer
; Returns value bound to variable
(define lookup-layer
  (lambda (var layer ref?)
    (cond
      ((null? (car layer)) '())
      ((eq? var (first-var layer)) (first-value layer ref?))
      (else (lookup-layer var (rest-layer layer) ref?)))))

(define lookup-pointer
  (lambda (var e)
    (cond 
      ((null? e) '())
      ((exists-binding? var (peek-layer e)) (lookup-pointer-layer var (peek-layer e)))
      (else (lookup-pointer var (pop-layer e))))))

(define lookup-pointer-layer
  (lambda (var layer)
    (cond
      ((null? (car layer)) '())
      ((eq? var (first-var layer)) (first-pointer layer))
      (else (lookup-pointer-layer var (rest-layer layer))))))

(define first-pointer
  (lambda (layer)
    (cond 
      ((null? (car layer)) '())
      (else (car (car (cdr layer)))))))


; Returns first variable name in the layer
(define first-var
  (lambda (layer)
    (cond 
      ((null? (car layer)) '())
      (else (car (car layer))))))

; Returns first value in the layer
(define first-value
  (lambda (layer ref?)
    (cond 
      ((null? (car layer)) '())
      (ref? (car (car (cdr layer))))
      (else (unbox (car (car (cdr layer))))))))

; Returns first value box in the layer
(define first-value-box
  (lambda (layer)
    (cond 
      ((null? (car layer)) '())
      (else (car (car (cdr layer)))))))

; Returns the first layer of the environment
(define peek-layer
  (lambda (e)
    (car e)))

; Adds a layer to the environment
; Returns new environment
(define push-layer
  (lambda (e)
    (cons '(() ()) e)))

; Removes a layer from the environment
; Returns new environment
(define pop-layer
  (lambda (e)
    (cdr e)))

; Removes the first element of the layer 
; Returns the new layer
(define rest-layer
  (lambda (layer)
    (cond
      ((null? layer) '())
      (else (cons (cdr (get-vars layer)) (cons (cdr (get-vals layer)) '()))))))

; Returns true if there is a binding for the variable in the layer
(define exists-binding?
  (lambda (var layer)
    (cond
      ((null? (car layer)) #f)
      ((eq? (first-var layer) var) #t)
      (else (exists-binding? var (rest-layer layer))))))

; Returns all of the variables in the layer
(define get-vars
  (lambda (layer)
    (car layer)))

; Returns all of the values in the layer
(define get-vals
  (lambda (layer)
    (car (cdr layer))))


