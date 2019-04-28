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
(check-equal? (try (define id (λ (x) x))
                   (define id2 (λ (y) (id y)))
                   (id2 z))
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

(check-equal? (try (assert (λ (x y) x)))
              "")
(check-equal? (try (assert ¬ (λ (x y) y)))
              "")
(check-equal? (try (assert not (λ (x y) y)))
              "")
(check-equal? (try (assert (λ (x y) y)))
              "expected true but got false\n")
(check-equal? (try (assert ¬ (λ (x y) x)))
              "expected false but got true\n")
(check-equal? (try (assert not (λ (x y) x)))
              "expected false but got true\n")

(check-equal? (try 2 3 11)
              "church number: 2\nchurch number: 3\nchurch number: 11\n")

(check-equal?
 (try
  (define Y (λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))))
  ((Y (λ (f) (λ (x) (x (f #false) 1)))) #true))
 "church number: 1\n")

(check-equal?
 (try (((λ (b tt ff) (b tt ff)) (λ (a) (λ (b) b)) (λ (x) x) (λ (x) x)) 42))
 "church number: 42\n")

(check-equal?
 (try (((λ (b tt ff) (b tt ff)) #f (λ (x) x) (λ (x) x)) 42))
 "church number: 42\n")
