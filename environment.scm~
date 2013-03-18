; The environment for the interpreter
; Return a new environment with true and false defined
(define new-environment
  (lambda ()
    (bind 'FALSE #f (bind 'TRUE #t (bind 'false #f (bind 'true #t '()))))))

; Bind the variable to the value in the environment
; returns null
(define bind
  (lambda (var value environment)
    (cons (cons var (cons value '())) environment)))

; Removes the binding if there is one
; returns null
(define remove-binding
  (lambda (var environment)
    (cond
      ((null? environment) '())
      ((eq? var (get-first-var environment)) (remove-binding var (cdr environment)))
      (else (cons (car environment) (remove-binding var (cdr environment)))))))

; Looks up binding in environment
; returns value bound to variable
(define lookup
  (lambda (var environment)
    (cond
      ((null? environment) '())
      ((eq? var (get-first-var environment)) (get-first-value environment))
      (else (lookup var (cdr environment))))))

; Gets first variable name in the environment
(define get-first-var
  (lambda (environment)
    (cond 
      ((null? environment) '())
      (else (car (car environment))))))

; Gets first variable name in the environment
(define get-first-value
  (lambda (environment)
    (cond 
      ((null? environment) '())
      (else (car (cdr (car environment)))))))
    
