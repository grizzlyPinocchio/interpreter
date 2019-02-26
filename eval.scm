(load "environment.scm")
(load "expressions.scm")

;Eval & apply
(define (evaluate expr env)
    ;(newline)
    ;(display "EVALUATE: ")
    ;(display expr)
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

(define (appl proc arguments env)
    ;(newline)
    ;(display "APPL: ")
    ;(display proc)
    ;(display " TO: ")
    ;(display arguments)
    (let ((eval-proc (evaluate proc env)))
         (cond ((primitive-proc? eval-proc) 
                (apply-primitive 
                    eval-proc 
                    (map (lambda (arg) (evaluate arg env))
                         arguments)))
               ((compound-proc? eval-proc)
                (let ((extended 
                       (extend-env
                            (env-of eval-proc)
                            (variables-of eval-proc)
                            (map (lambda (arg) (evaluate arg (env-of eval-proc)))
                                 arguments))))
                     (evaluate (body-of eval-proc) extended))) 
               (else (begin
                        (newline)
                        (display ("Unknown procedure '"))
                        (display proc)
                        (display "' passed to appl")
                        'error)))))

(define (evaluate-sequ expr env)
    (cond ((empty? expr) 
           (display "Error - null list given to evaluate-sequ")) 
          ((empty? (rest-expr expr)) 
           (evaluate (first-expr expr) env))
          (else (begin (evaluate (first-expr expr) env)
                       (evaluate-sequ (rest-expr expr) env)))))

;Expression
(define (do-primitive expr env) expr)

(define (do-definition expr env)
    (if (not (proc-definition? expr))
        (env 'define (definition-var expr) (definition-val expr))
        (env 'define (proc-definition-var expr) (expr-to-lambda expr env))))

(define (do-variable expr env)
    (env 'lookup (expr-to-variable expr)))

(define (do-lambda expr env) 
    (if (equal? (length expr) 4)
        expr
        (make-lambda (body-of expr) (variables-of expr) env)))

(define (do-application expr env)
    ;(newline)
    ;(display "DO-APPLICATION: ")
    ;(display expr)
    (if (lambda? (proc-name expr))
        (appl (proc-name expr)
              (map (lambda (arg) (evaluate arg env))
                   (lambda-to-args expr))
              env)
        (appl (evaluate (proc-name expr) env) 
              (map (lambda (arg) (evaluate arg env)) (expr-to-args expr))
              env)))

;Procedure 
(define (make-lambda body variables env) (list 'lambda variables body env))

(define (body-of proc) (caddr proc))

(define (variables-of proc) (cadr proc))

(define (env-of proc) (cadddr proc)) 

(define (expr-to-lambda expr env)
    (make-lambda (expr-to-body expr) (expr-to-variables expr) env)) 

;Assignment

(define (do-assignment expr env)
    (let ((variable (assign-variable expr))
          (new-definition (evaluate (assign-definition expr) env)))
         (env 'assign! variable new-definition)
         'assignment-done))

;Lambda

;While

(define (do-while-orig expr env)
    (let ((halt-proc (halt-lambda-of expr))
          (exec-proc (exec-lambda-of expr)))
         ;(newline)
         ;(display "HALT-PROC: ")
         ;(display halt-proc)
         ;(newline)
         ;(display "EXEC-PROC: ")
         ;(display exec-proc)
         ;(newline)
         ;(display "a: ")
         ;(display (evaluate 'a main))
         (if (appl halt-proc '() env)
             (begin (appl exec-proc '() env)
                    (do-while expr env))
             'while-done)))

(define (do-while expr env)
    (if (evaluate (cadr expr) env)
        (begin (evaluate-sequ (cddr expr) env)
               (do-while expr env))
        'while-done))

;Environment
(define (extend-env env vars args)
    (define extended 
      (make-environment (string-append "extend-" (env 'name)) env)) 
    (map (lambda (var arg) (extended 'define var arg)) vars args)
    extended)

;Application

(define (lambda-proc? proc)
    (if (not (pair? proc))
        #F
        (equal? (car proc) 'lambda)))
