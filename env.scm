(define a 2)

(define (times-a x) (* a x))

(define (return-value)
    (define a 3)
    (newline) (display (* 1 a)) (newline) 
    (times-a 1))

(newline)
(display (times-a 1))
(newline)
(display (return-value))
