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
    (eval `(,#'module ,modname lc ,@args)
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
(check-equal? (try (λ (x) (λ (y) x)))
              "church boolean: #t\n")
(check-equal? (try (λ (x) (λ (y) y)))
              "church number: 0\nchurch boolean: #f\n")
