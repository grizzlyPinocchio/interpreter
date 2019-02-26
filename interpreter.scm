(load "eval.scm")

(define (repl)
    (read-eval-print-aux (make-environment "main" '())))

(define (read-eval-print-aux env)
    (display "<= ")
    (let ((input (read)))
         (if (equal? input 'exit)
             'exiting
             (begin (display "=> ") 
                    (display (evaluate input env))
                    (newline)
                    (read-eval-print-aux env)))))
