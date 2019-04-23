#lang racket/base
(module reader syntax/module-reader lc)

(require (for-syntax racket/base)
         racket/promise)
(provide
 (rename-out
  [datum #%datum]
  [top-interaction #%top-interaction]
  [top #%top]
  [module-begin #%module-begin]
  [-λ λ]
  [app #%app]))

(define-syntax (app stx)
  (syntax-case stx ()
    [(_ f x)
     #'((check-proc f) (delay x))]))

(struct exn:fail:not-a-function exn:fail ())
(define (check-proc f)
  (unless (procedure? f)
    (raise (exn:fail:not-a-function
            (format "expected a procedure, got ~e" f)
            (current-continuation-marks))))
  f)

(define-for-syntax (do-force id)
  (λ (stx)
    (syntax-case stx ()
      [x
       (identifier? #'x)
       #`(force* #,id)]
      [(a b)
       #`(app (force* #,id) b)])))

;; this is buggy; need a test case!
(define (force* x)
  (force x)
  #;
  (if (promise? x) (force* (force x)) x))

(define-syntax (-λ stx)
  (syntax-case stx ()
    [(_ (x) e)
     (with-syntax ([(y) (generate-temporaries #'(x))])
       (syntax/loc stx
         (λ (y)
           (let-syntax ([x (do-force #'y)])
             e))))]))

(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . datum)
     (raise-syntax-error
      'lc
      "no literals!" #'datum)]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ args ...)
     #'(#%plain-module-begin
        (begin (print-it args)) ...)]))

(define (get-number x)
  (let/ec k
    (with-handlers ([exn:fail:not-a-function? (λ (x) #f)])
      (cond
        [(procedure? x)
         (define pa (x (λ (x) (force-and-add1 k x))))
         (cond
           [(procedure? pa)
            (define n (pa 0))
            (cond
              [(number? n) n]
              [else #f])]
           [else #f])]
        [else #f]))))

(define (force-and-add1 k arg)
  (define n (force* arg))
  (if (number? n)
      (+ n 1)
      (k #f)))

(define (get-boolean x)
  (with-handlers ([exn:fail:not-a-function? (λ (x) "nope!")])
    (cond
      [(procedure? x)
       (define true-value (gensym 'true))
       (define pa (x true-value))
       (cond
         [(procedure? pa)
          (define false-value (gensym 'false))
          (define pb (pa false-value))
          (cond
            [(equal? pb true-value) #t]
            [(equal? pb false-value) #f]
            [else "nope!"])]
         [else "nope!"])]
      [else "nope!"])))

(define (print-it x)
  (define n (get-number x))
  (define b (get-boolean x))
  (when (number? n) (printf "church number: ~a\n" n))
  (when (boolean? b) (printf "church boolean: ~a\n" b))
  (unless (or (number? n) (boolean? b))
    (printf "~s\n" x)))

(define-syntax (top stx)
  (syntax-case stx ()
    [(_ . x) #''x]))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . x) #'(print-it x)]))
