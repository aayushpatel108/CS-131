#lang racket

(provide (all-defined-out))
(define LAMBDA (string->symbol "\u03BB"))

(define (valid-lambda-expression? exp)
  (and (equal? (car exp) 'lambda) (not (empty? (cdr exp))) (list? (cadr exp)))
)

(define (rename-var old-name exp)
  (cond
    [(equal? exp empty) empty]
    [(equal? (car exp) 'quote) exp]
    [(valid-lambda-expression? exp) (if (and (list? (cdr exp)) (member old-name (cadr exp)))
                                        exp
                                       (cons (car exp) (cons (cadr exp) (rename-var old-name (cddr exp)))))]
    [(list? (car exp)) (cons (rename-var old-name (car exp)) (rename-var old-name (cdr exp)))]
    [(equal? (car exp) old-name) (cons 'x (rename-var old-name (cdr exp)))]
    [else (cons (car exp) (rename-var old-name (cdr exp)))]
  )
 )

(define (fnx x)
  (let ([x-var (cadr x)]
        [x-exp  (cddr x)])
    (cons 'lambda (cons '(x) (rename-var (car x-var) x-exp)))
   ))