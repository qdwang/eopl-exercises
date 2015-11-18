#lang typed/racket

(define-type Exp (Rec exp (U Symbol Integer Null (Listof exp))))

(: is-call? (-> Exp Boolean))
(define (is-call? exp)
  (if (list? exp)
      (if (member (car exp) '(lambda if zero? + - * / K))
          #f
          #t)
      #f))

(: is-tail? (-> Exp Integer Boolean))
(define (is-tail? exp position)
  (match exp
    [(list 'lambda _ _)
     (if (= 2 position) #t #f)]
    [(list 'if _ _ _)
     (if (> 1 position) #t #f)]
    [_
     (if (= 0 position) #t #f)]))

(: tail-cps (-> Exp Exp))
(define (tail-cps exp)
  (if (is-call? exp)
      (append (cast exp (Listof Exp)) '(K))
      `(K ,exp)))

(: inner-cps (-> Exp Exp))
(define (inner-cps exp)
  
  (define v-num (cast 0 Integer))
  (define prefix-exp '())
  
  (: _cps (-> Exp Exp))
  (define (_cps exp)
    (if (list? exp)
        (let ([inner-lst (map (lambda (x) (_cps (cast x Exp))) exp)])
          (begin '(print inner-lst) '(print (newline))
          (if (is-call? inner-lst)
              (begin (set! v-num (+ v-num 1))
                     (let ([f-key (string->symbol (string-append "v" (number->string v-num)))])
                       (begin (set! prefix-exp `(,prefix-exp ,inner-lst (lambda (,f-key) )))
                              f-key)))
              inner-lst)))
        exp))
  (let ([base-exp (_cps exp)])
    (begin (print prefix-exp)
           base-exp)))
           
  



; for test
(define test-a
  '(+ 22 (- x 3) x))

(define test-b
  '(+ 22 (f x) 37))

(define test-c
  '(g 22 (f x)))

(define test-d
  '(+ 22 (f (g x)) 33 (g y)))

(inner-cps (tail-cps test-a))
(inner-cps (tail-cps test-b))
(inner-cps (tail-cps test-c))
(inner-cps (tail-cps test-d))