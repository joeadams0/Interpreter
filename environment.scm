; The environment for the interpreter
; Return a new environment
(define new-environment
  (lambda ()
    '(
      ()
      ()   )))
;(define new-environment
;  (lambda ()
;    (bind 'FALSE #f (bind 'TRUE #t (bind 'false #f (bind 'true #t (push-layer '())))))))


 
(define new-class
  (lambda ()
    '(
      (()())
      ()
      ()
      ()  )))

; DANIEL
; Class structure -> (((static var names) (static var values)) ((method names)(method closures)) (parent) (instance variable names))
; Sets the parent of the class
; (set-parent 'parent (()()()())) -> (()()(parent)())
; Returns the new class
; Dont use boxes
(define set-parent
  (lambda (parent class)
    (cons 
     (car class) 
     (cons 
      (caar class) 
      (cons (enlist parent) (cdddr class))))))

; enlist takes an item and wraps it in parenthases,
; essentially returning the list containing the itme
(define enlist
  (λ (item)
    (cons item '())))

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

; Adds a variable name to the instance-variable list
; (set-instance-variable 'poop '((()())()()())) -> ((()())()()(poop))
; Return new class
; No environment
(define set-instance-variable 
  (lambda (var-name class)
    1))

; Adds a method to the class
; (bind-method 'poop (poop-closure) '(()(()())()())) -> (()((poop)(poop-closure))()())
; Return class
; Dont use boxes
(define bind-method
  (lambda (method-name closure class)
    1))

; Looks up a variable in a class
; (lookup 'poop (((poop)(#&10))()()()) '()) -> 10
; Returns the UNBOXED value of the variable
; Ignore instance for now
(define lookup-var
  (lambda (var class instance)
    1))

; Looks up a method closure in a class
; (lookup 'poop (()((poop)(poop-closure))()()))  -> poop-closure
; Returns the closure
(define lookup-method
  (lambda (var class)
    1))

; Adds a new class to the main environment
; Takes:
;    class-name:    the new class's name
;    class-body:    the closure of the class being added
;    e:             the environment to which this class is being added.
; Returns the new environment
;(define bind-class
;  (λ (class-name class-body e)
;    (

; Bind the variable to the value in the environment
; Returns the new environment
(define bind
  (lambda (var value environment)
    (cons (cons (cons var (get-vars (peek-layer environment))) 
                (cons (cons (box value) (get-vals (peek-layer environment))) '())) 
          (pop-layer environment))))

(define bind-pointer
  (lambda (var value environment)
    (cons (cons (cons var (get-vars (peek-layer environment))) (cons (cons value (get-vals (peek-layer environment))) '())) (pop-layer environment))))

; Updates the value of the binding
; Returns the value
(define update-binding
  (lambda (var value e)
    (cond
      ((null? e) (error 'update-binding "Variable not declared"))
      ((null? (car (peek-layer e))) (update-binding var value (pop-layer e)))
      ((eq? var (first-var (peek-layer e))) (set-box! (first-value-box (peek-layer e)) value) value)
      (else (update-binding var value (cons (rest-layer (peek-layer e)) (pop-layer e)))))))
       
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
                                                           (k (cons (cons (first-var layer) (get-vars layer2)) (cons (cons (box (first-value layer)) (get-vals layer2)) '())))))))))

; Looks up binding in the environment
; Returns the value bound to the variable
(define lookup
  (lambda (var e)
    (cond
      ((null? e) '())
      ((exists-binding? var (peek-layer e)) (lookup-layer var (peek-layer e)))
      (else (lookup var (pop-layer e))))))

; Looks up binding in the layer
; Returns value bound to variable
(define lookup-layer
  (lambda (var layer)
    (cond
      ((null? (car layer)) '())
      ((eq? var (first-var layer)) (first-value layer))
      (else (lookup-layer var (rest-layer layer))))))

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
  (lambda (layer)
    (cond 
      ((null? (car layer)) '())
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


