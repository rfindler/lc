#lang racket
(require rackunit)
(define ns (make-base-namespace))

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ args ...)
     (with-syntax ([line-number (syntax-line stx)])
       #'(try/proc line-number 'args ...))]))

(define (try/proc line-number . args)
  (define sp (open-output-string))
  (parameterize ([current-output-port sp])
    (define modname
      (string->symbol (format "m~a" line-number)))
    (eval `(,#'module ,modname lc (#%module-begin ,@args))
          ns)
    (namespace-require `',modname ns))
  (get-output-string sp))

(check-equal? (try z)
              "z\n")

(check-equal? (try ((λ (x) x) z))
              "z\n")

(check-equal? (try (((λ (x) (λ (y) x)) z) q))
              "z\n")

(check-equal? (try (((λ (f) (λ (x) (f x)))
                     (λ (x) x))
                    z))
              "z\n")

(check-equal? (try (((λ (f) (λ (x) (f (f x))))
                     (λ (x) x))
                    z))
              "z\n")

(check-equal? (try (λ (f) (λ (x) (f (f (f (f x)))))))
              "church number: 4\n")
(check-equal? (try (λ (f x) (f (f (f (f (f x)))))))
              "church number: 5\n")
(check-equal? (try (λ (x) (λ (y) x)))
              "church boolean: #t\n")
(check-equal? (try (λ (x y) x))
              "church boolean: #t\n")
(check-equal? (try (λ (x) (λ (y) y)))
              "church number: 0\nchurch boolean: #f\n")

(check-equal? (try (define id (λ (x) x))
                   (id z))
              "z\n")

(check-equal? (try ((λ (x y z) y) p q r))
              "q\n")
(check-equal? (try ((λ (f) (f p q r))
                    (λ (p q r) q)))
              "q\n")

(check-equal? (try (= (λ (x) x)
                      (λ (x) x)))
              "")

(check-equal? (try (= (λ (f x) (f x))
                      (λ (f x) x)))
              "the numbers are not equal, got 1 and 0\n")
