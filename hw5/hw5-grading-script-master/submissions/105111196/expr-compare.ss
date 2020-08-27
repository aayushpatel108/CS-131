#lang racket
(provide (all-defined-out))
(define LAMBDA (string->symbol "\u03BB"))

;Followed Xinyu's Hint on Piazza for Lambda Expressions
(define (expr-compare x y)
  (cond
    [(and (list? x) (list? y)) (list-compare x y)]
    [(and (boolean? x) (boolean? y)) (boolean-compare x y)]
    [else (basic-compare x y)]
   )
)

(define (basic-compare x y)
  (cond
    [(equal? x y) x]
    [else (list 'if '% x y)]
  )
)
(define (boolean-compare x y)
  (cond
    [(equal? x y) x]
    [(equal? x #t) '%]
    [else '(not %)]
   )
)
;taken from TA Hint Code
(define (lambda? x)
  (member x '(lambda λ))
)

(define (valid-lambda-expression? exp)
  (and (lambda? (car exp)) (not (empty? (cdr exp))) (list? (cadr exp)))
)
(define (rename-var old-name new-name exp)
  (cond
    [(equal? exp empty) empty]
    [(equal? (car exp) 'quote) exp]
    [(valid-lambda-expression? exp) (if (and (list? (cdr exp)) (member old-name (cadr exp)))
                                        exp
                                       (cons (car exp) (cons (cadr exp) (rename-var old-name new-name (cddr exp)))))]
    [(list? (car exp)) (cons (rename-var old-name new-name (car exp)) (rename-var old-name new-name (cdr exp)))]
    [(equal? (car exp) old-name) (cons new-name (rename-var old-name new-name (cdr exp)))]
    [else (cons (car exp) (rename-var old-name new-name (cdr exp)))]
  )
)
(define (rename-args old-names new-names exp)
  (cond
    [(or (equal? old-names empty) (equal? new-names empty)) exp]
    [else (rename-args (cdr old-names) (cdr new-names) (rename-var (car old-names) (car new-names) exp))]
  )
)
(define (bind x y)
  (if (equal? x y) x (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
)
(define (get-bindings x-vars y-vars)
  (if (equal? x-vars empty) empty (cons (bind (car x-vars) (car y-vars)) (get-bindings (cdr x-vars) (cdr y-vars))))
)
(define (get-correct-lambda x y)
  (if (equal? x y) x LAMBDA)
)
(define (lambda-compare x y)
  (let ([x-vars (cadr x)]
        [y-vars (cadr y)]
        [x-exp  (cddr x)]
        [y-exp  (cddr y)])
    (if (equal? (length x-vars) (length y-vars))
        (let
          ([bindings (get-bindings x-vars y-vars)])
          (let 
              ([new-exp (expr-compare (rename-args x-vars bindings x-exp) (rename-args y-vars bindings y-exp))])
              (append (cons (get-correct-lambda (car x) (car y)) (cons bindings empty)) new-exp)
          )
        )
        (basic-compare x y)
    )
  )
)
(define (list-compare x y)
  (cond
    [(equal? x empty) empty]
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (basic-compare x y)]
    [(and (not (equal? (car x) (car y))) (or (equal? (car x) 'if) (equal? (car y) 'if))) (basic-compare x y)]
    [(and (valid-lambda-expression? x) (valid-lambda-expression? y)) (lambda-compare x y)]
    [(or (lambda? (car x)) (lambda? (car y))) (basic-compare x y)]
    [else (list-traverse x y)]
  )
)
(define (list-traverse x y)
  (cond
    [(equal? x empty) '()]
    [(equal? y empty) '()]
    [(equal? (length x) (length y)) (cons (expr-compare (car x) (car y)) (list-traverse (cdr x) (cdr y)))]
    [else (basic-compare x y)]
  )
)

;taken from TA hint code
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define (test-expr-x)
  `( #f #t (cons 1 2) (quote a) (if (c) (+ d 2) (list "string1" "string2") (lambda (a) ((λ (b) (f a b))) 12)))
)

(define (test-expr-y)
  `( #t #f (cons 3 4) 'b (if (i) (+ j 4) (list "string1" "string2") (λ (x) ((lambda (y) (g x y))) 13)))
)

(define (test)
  (test-expr-compare test-expr-x test-expr-y)
 )
