;Entry
(define (make-entry key value) (cons key value))

(define (key-of entry) (car entry))

(define (value-of entry) (cdr entry))

;Lookup-table
(define (make-lookup-table my-name)
    (let ((lookup-table (list))
          (name my-name))

         (define (first-entry table) (car table))

         (define (rest table) (cdr table))

         (define (lookup key)
            (define (lookup-aux value table)
                (cond ((null? table)
                       (begin
                        (newline)
                        (display "Value '")
                        (display value)
                        (display "' not found in table '")
                        (display my-name)
                        (display "'")
                        'lookup-failed))
                      ((equal? key (key-of (first-entry table)))
                       (value-of (first-entry table)))
                      (else (lookup-aux value (rest table)))))
            (lookup-aux key lookup-table))
         
         (define (insert! key value) 
            (set! lookup-table
                  (cons (make-entry key value) lookup-table))
            'insert-done)

         (define (delete! key)
            (define (delete-aux key table)
                (cond ((null? table)
                      '())
                      ((equal? key (key-of (first-entry table)))
                       (rest table))
                      (else (cons (first-entry table) (rest table)))))
            (set! lookup-table (delete-aux key lookup-table))
            'delete-done) 

         (define (display-table)
            (newline)
            (display lookup-table)
            'display-done)

         (define (dispatch msg . args)
            (cond ((equal? msg 'delete!) (apply delete! args))
                  ((equal? msg 'insert!) (apply insert! args))
                  ((equal? msg 'lookup) (apply lookup args))
                  ((equal? msg 'display) (display-table))
                  (else (begin
                            (newline)
                            (display "Unknown message '")
                            (display msg)
                            (display "' dispatched to lookup-table '")
                            (display name)
                            (display "'")))))
         dispatch))
