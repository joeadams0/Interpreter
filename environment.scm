; The environment for the interpreter
; Return a new environment with true and false defined
(define new-environment
  (lambda ()
    (bind 'FALSE #f (bind 'TRUE #t (bind 'false #f (bind 'true #t (push-layer '())))))))

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


