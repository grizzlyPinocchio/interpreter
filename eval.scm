(load "environment.scm")
(load "expressions.scm")

;Eval & apply
(define (evaluate expr env)
    (newline)
    (display "EVALUATE: ")
    (display expr)
    (cond ((primitive? expr) (do-primitive expr env))
          ((definition? expr) (do-definition expr env))
          ((variable? expr) (do-variable expr env))
          ((lambda? expr) (do-lambda expr env))
          ((assignment? expr) (do-assignment expr env))
          ((while? expr) (do-while expr env))
          ((application? expr) (do-application expr env))
          (else (begin
                    (newline)
                    (display "Unknown expression '")
                    (display expr)
                    (display "' passed to evaluate")
                    'error))))

(define (apply-proc proc arguments)
    (newline)
    (display "APPLY-PROC: ")
    (display proc)
    (display " TO: ")
    (display arguments)
    (cond ((primitive-proc? proc) 
           (apply-primitive proc 
                            (map (lambda (arg) (evaluate arg (env-of proc)))
                                 arguments)))
          ((compound-proc? proc)
           (let ((extended (extend-env
                             (env-of proc)
                             (variables-of proc)
                             (map (lambda (arg) (evaluate arg (env-of proc)))
                                  arguments))))
                (evaluate (body-of proc) extended))) 
          (else (begin
                    (newline)
                    (display ("Unknown procedure '"))
                    (display proc)
                    (display "' passed to apply-proc")
                    'error))))

;Expression
(define (do-primitive expr env) expr)

(define (do-definition expr env)
    (if (not (proc-definition? expr))
        (env 'define (definition-var expr) (definition-val expr))
        (env 'define (proc-definition-var expr) (expr-to-proc expr env))))

(define (do-variable expr env)
    (env 'lookup (expr-to-variable expr)))

(define (do-lambda expr env) expr)

(define (do-application expr env)
    (newline)
    (display "DO-APPLICATION: ")
    (display expr)
    (if (lambda? (proc-name expr))
        (apply-proc (lambda-to-proc (proc-name expr) env)
                    (map (lambda (arg) (evaluate arg env))
                         (lambda-to-args expr)))
        (apply-proc (env 'lookup (proc-name expr)) 
                    (map (lambda (arg) (evaluate arg env))
                         (expr-to-args expr)))))

;Procedure 
(define (make-proc body variables env) (list 'proc variables body env))

(define (body-of proc) (caddr proc))

(define (variables-of proc) (cadr proc))

(define (env-of proc) (cadddr proc)) 

(define (expr-to-proc expr env)
    (make-proc (expr-to-body expr) (expr-to-variables expr) env)) 

;Assignment

(define (do-assignment expr env)
    (let ((variable (assign-variable expr))
          (new-definition (evaluate (assign-definition expr) main)))
         (env 'assign! variable new-definition)
         'assignment-done))

;Lambda
(define (lambda-to-proc expr env)
    (make-proc (lambda-body expr) (lambda-variables expr) env))

;While

(define (do-while expr env)
    (let ((halt-proc (lambda-to-proc (halt-lambda-of expr) env))
          (exec-proc (lambda-to-proc (exec-lambda-of expr) env)))
         (newline)
         (display "HALT-PROC: ")
         (display halt-proc)
         (newline)
         (display "EXEC-PROC: ")
         (display exec-proc)
         (newline)
         (display "a: ")
         (display (evaluate 'a main))
         (if (apply-proc halt-proc '())
             (begin (apply-proc exec-proc '())
                    (do-while expr env))
             'while-done)))

;Environment
(define (extend-env env vars args)
    (define extended 
      (make-environment (string-append "extend-" (env 'name)) env)) 
    (map (lambda (var arg) (extended 'define var arg)) vars args)
    extended)

;Application

(define (primitive-proc? proc)
    (cond ((add-expr? (body-of proc)) True)
          ((sub-expr? (body-of proc)) True)
          ((mul-expr? (body-of proc)) True)
          ((div-expr? (body-of proc)) True)
          ((bool-eq-expr? (body-of proc)) True)
          ((bool-gt-expr? (body-of proc)) True)
          ((bool-lt-expr? (body-of proc)) True)
          (else False)))

(define (apply-primitive proc args) 
    (cond ((add-expr? (body-of proc)) (apply + args))
          ((sub-expr? (body-of proc)) (apply - args))
          ((mul-expr? (body-of proc)) (apply * args))
          ((div-expr? (body-of proc)) (apply / args))
          ((bool-eq-expr? (body-of proc)) (apply = args))
          ((bool-gt-expr? (body-of proc)) (apply > args))
          ((bool-lt-expr? (body-of proc)) (apply < args))
          (else (begin
                    (newline)
                    (display "Unknown proc '")
                    (display proc)
                    (display "' passed to apply-primitive")
                    'error))))

(define (compound-proc? proc) ;lol - should be changed
    (not (primitive-proc? proc)))

;Testing
(define main (make-environment "main" '()))
(define expr1 '(define a 5))
(define expr2 '(define b 7))
(define expr3 '(define (mul x y) (* x y)))
(define expr4 '(define (mul2 x y) (mul (mul x y) a)))
(define expr5 '(mul2 2 3))
(evaluate '(define (* x y) (* x y)) main)
(evaluate '(define (+ x y) (+ x y)) main)
(evaluate '(define (add x y) (+ x y)) main)
(evaluate '(define (eq? x y) (= x y)) main)
(evaluate '(define (gt? x y) (> x y)) main) 
(evaluate '(define (lt? x y) (< x y)) main)
(evaluate expr1 main)
(evaluate expr2 main)
(evaluate expr3 main)
(evaluate expr4 main)
(newline)
;(display (evaluate expr5 main))
;(main 'display)
(newline)
(evaluate '(assign! a 3) main)
(define ans (evaluate '((lambda () (gt? a 2))) main))
(newline)
(display ans)
(evaluate '((lambda () (assign! a 0))) main)
(newline)
(evaluate '(while (lt? a 10) (assign! a (add 2 a))) main)
(display (evaluate 'a main))
;(display (apply-proc '(proc (x) (mul x 2) main) '(10))) 
