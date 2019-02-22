;Generic
(define True #T)
(define False #F)

(define (boolian? x) (or (equal? True x) (equal? False x)))

(define (tagged-list? expr tag)
    (if (and (pair? expr)
             (equal? (car expr) tag))
        True
        False))

;Definiton
(define (definition? expr)
    (or (and (tagged-list? expr 'define)
             (equal? (length expr) 3))))

(define (definition-var expr) (cadr expr))

(define (definition-val expr) (caddr expr))

;Assignment

(define (assignment? expr) (tagged-list? expr 'assign!))

(define (assign-variable expr) (cadr expr))

(define (assign-definition expr) (caddr expr))

;Procedure Definition

(define (proc-definition? expr)
    (and (definition? expr)
         (pair? (cadr expr))))

(define (proc-definition-var expr) (caadr expr))

(define (expr-to-body expr) (caddr expr))

(define (expr-to-variables expr) (cdadr expr)) 

;Procedure Application

(define (expr-to-args expr) (cdr expr))

(define (proc-name expr) (car expr))

;While

(define (while? expr) (tagged-list? expr 'while))

(define (halt-lambda-of expr) (make-lambda '() (cadr expr)))

(define (exec-lambda-of expr) (make-lambda '() (caddr expr)))

;Variables
(define (variable? expr) (symbol? expr))

(define (expr-to-variable expr) expr) 

;Primitives

(define (primitive? expr)
    (cond ((string? expr) True)
          ((number? expr) True)
          ((boolian? expr) True)
          ((equal? expr '*) True)
          ((equal? expr '+) True)
          ((equal? expr '-) True)
          ((equal? expr '/) True)
          (else False)))

(define (do-primitive expr) expr)

(define (add-expr? expr) (equal? (car expr) '+))

(define (mul-expr? expr) (equal? (car expr) '*))

(define (sub-expr? expr) (equal? (car expr) '-))

(define (div-expr? expr) (equal? (car expr) '/))

(define (bool-eq-expr? expr) (equal? (car expr) '=))

(define (bool-gt-expr? expr) (equal? (car expr) '>))

(define (bool-lt-expr? expr) (equal? (car expr) '<))

;Lambda 
;lambda-expr has form ('lambda variables body)

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-variables expr) (cadr expr))

(define (lambda-body expr) (caddr expr))

;returns ARGS in ((lambda (x) (body)) ARGS) 
(define (lambda-to-args expr) (cdr expr))

(define (make-lambda args body) (list 'lambda args body))

;Application

(define (application? expr) 
    (and (pair? expr) 
         (or (symbol? (car expr))
             (lambda? (car expr)))))
