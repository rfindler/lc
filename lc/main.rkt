#lang racket/base
(module reader syntax/module-reader lc)

(require (for-syntax racket/base
                     syntax/stx
                     syntax/parse)
         racket/promise)
(provide
 (rename-out
  [-define define]
  [datum #%datum]
  [top-interaction #%top-interaction]
  [top #%top]
  [module-begin #%module-begin]
  [-λ λ]
  [app #%app]
  [-= =]
  [-assert assert]))

(define-syntax (app stx)
  (syntax-case stx ()
    [(_ f x)
     #'((check-proc f) (delay x))]
    [(_ f x y z ...)
     #'(app (app f x) y z ...)]))

(struct exn:fail:not-a-function exn:fail ())
(define (check-proc f)
  (unless (procedure? f)
    (if (symbol? f)
        (raise (exn:fail:not-a-function
                (format "do not know what function the free variable `~a` stands for" f)
                (current-continuation-marks)))
        (raise (exn:fail:not-a-function
                (format "expected a procedure, got ~e" f)
                (current-continuation-marks)))))
  f)

(define-for-syntax (do-force id)
  (λ (stx)
    (syntax-case stx ()
      [x
       (identifier? #'x)
       #`(force* #,id)]
      [(a b ...)
       #`(app (force* #,id) b ...)])))

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
             e))))]
    [(_ (x y z ...) e) #'(-λ (x) (-λ (y z ...) e))]))

(define-syntax (datum stx)
  (syntax-parse stx
    [(_ . datum)
     (define raw-datum (syntax-e #'datum))
     (cond
       [(exact-nonnegative-integer? raw-datum)
        #`(to-church-numeral #,raw-datum)]
       [(equal? raw-datum #false) #'(λ (x) (λ (y) y))]
       [(equal? raw-datum #true) #'(λ (x) (λ (y) x))]
       [else
        (raise-syntax-error
         'lc
         "no literals (except natural numbers, #true, and #false)" #'datum)])]))

(define (to-church-numeral n)
  (-λ (f)
    (-λ (x)
      (for/fold ([x x])
                ([i (in-range n)])
        (app f x)))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ arg ...)
     #'(#%plain-module-begin (nest-them arg ...))]))

(define-syntax (nest-them stx)
  (syntax-parse stx
    [(_ . expressions)
     (define defined-names '())
     (define body
       (let loop ([expressions #'expressions])
         (syntax-parse expressions
           [() #'(void)]
           [(a . b)
            (syntax-parse #'a
              #:literals (-define)
              [(-define x e)
               (set! defined-names (cons #'x defined-names))]
              [_ (void)])
            #`(maybe-print-it repl-names-table a #,(loop #'b))])))
     (define dup (check-duplicate-identifier defined-names))
     (when dup
       (define dups
         (for/list ([d (in-list defined-names)]
                    #:when (bound-identifier=? d dup))
           d))
       (raise-syntax-error (syntax-e dup) "duplicate definition"
                           (car dups)
                           #f
                           (cdr dups)))
     (with-syntax ([(defined-names ...) defined-names])
       #`(begin
           (define repl-names-table (make-hash))
           (disallow-references (defined-names ...) #,body)
           (define defined-names (hash-ref repl-names-table 'defined-names)) ...
           ))]))

(define-for-syntax ((raise-error a) stx)
  (define sym (syntax-e a))
  (syntax-parse stx
    #:literals (set!)
    [x
     (identifier? #'x)
     (raise-syntax-error
      sym
      (format "cannot refer to ~a until after it is defined" sym)
      #'x)]
    [(x . y)
     (raise-syntax-error
      sym
      (format "cannot refer to ~a until after it is defined" sym)
      #'x)]))

(define-syntax (disallow-references stx)
  (syntax-parse stx
    [(_ () body) #'body]
    [(_ (a b ...) body)
     #'(let-syntax ([a (raise-error #'a)])
         (disallow-references (b ...) body))]))

(define-syntax (-define stx)
  (raise-syntax-error 'define "illegal use of define" stx))

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

(define-syntax (-= stx)
  (raise-syntax-error '= "illegal use of =" stx))

(define-syntax (-assert stx)
  (raise-syntax-error 'assert "illegal use of =" stx))

(require racket/stxparam)
(define-syntax-parameter line-of-define #f)

(define-syntax (maybe-print-it stx)
  (syntax-parse stx
    [(_ repl-names-table expr body)
     (syntax-parse #'expr
       #:literals (-define -= -assert)
       #:datum-literals (¬ not)
       [(-define x:id rhs)
        #'(let ([x rhs])
            (hash-set! repl-names-table 'x x)
            body)]
       [(-= a b)
        (with-syntax ([line-number (syntax-line #'expr)])
          #'(begin (=/proc line-number a b) body))]
       [(-= . whatever)
        (raise-syntax-error "malformed =" #'expr)]
       [(-assert e)
        (with-syntax ([line-number (syntax-line #'expr)])
          #'(begin (assert/proc line-number #t e) body))]
       [(-assert ¬ e)
        (with-syntax ([line-number (syntax-line #'expr)])
          #'(begin (assert/proc line-number #f e) body))]
       [(-assert not e)
        (with-syntax ([line-number (syntax-line #'expr)])
          #'(begin (assert/proc line-number #f e) body))]
       [(-assert . whatever)
        (raise-syntax-error "malformed assert" #'expr)]
       [e #'(begin (print-it/proc e) body)])]))

(define (assert/proc line-number true? a)
  (define bool (get-boolean a))
  (unless (boolean? bool)
    (error 'assert "expected a boolean, got ~s" a))
  (unless (equal? true? bool)
    (printf "expected ~a~a but got ~a\n"
            (if true? "true" "false")
            (if line-number
                (format " compared on line ~a" line-number)
                "")
            (if bool "true" "false"))))

(define (=/proc line-number a b)
  (define na (get-number a))
  (unless (number? na) (error '= "first argument is not a number ~s" a))
  (define nb (get-number b))
  (unless (number? nb) (error '= "second argument is not a number ~s" b))
  (unless (= na nb)
    (printf "the numbers~a are not equal, got ~a and ~a\n"
            (if line-number
                (format " compared on line ~a" line-number)
                "")
            na nb)))

(define (print-it/proc x)
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
  (syntax-parse stx
    #:literals (-define)
    [(_ . (-define . whatever))
     (raise-syntax-error 'define "not allowed at the top-level"
                         (stx-cdr stx))]
    [(_ . x)
     #'(maybe-print-it
        ;; we shouldn't use this because of the
        ;; previous case in the syntax-parse
        dont-use-me
        x (void))]))
