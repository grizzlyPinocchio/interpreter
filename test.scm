(load "eval.scm")

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
