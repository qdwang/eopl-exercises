#lang typed/racket

(define-type Exp (Rec exp (U Symbol Integer Null (Listof exp))))

(: is-call? (-> Exp Boolean))
(define (is-call? exp)
  (if (list? exp) 
      (if (member (car exp) '(lambda if zero? + - * / K sub1))
          #f
          #t)
      #f))

(: has-call? (-> Exp Boolean))
(define (has-call? exp)
  (if (list? exp)
      (if (is-call? exp)
          #t
          (if (member #t (map (lambda ([x : Exp]) (has-call? x)) exp))
              #t
              #f))
      #f))
     
(: tail-prepare (-> Exp Exp))
(define (tail-prepare exp)
  (if (is-call? exp)
      (append (cast exp (Listof Exp)) '(K))
      `(K ,exp)))

(: tail-cps (-> Exp Exp))
(define (tail-cps exp)
  
  (define v-num (cast 0 Integer))
  (define prefix-exp '())
  
  (: _cps (-> Exp Exp))
  (define (_cps exp)
    (if (list? exp)
        (if (equal? 'lambda (car exp))
            (lambda-cps exp)
            (let ([inner-lst (map (lambda ([x : Exp]) (_cps x)) exp)])
              (if (and (is-call? inner-lst) (not (equal? 'K (list-ref inner-lst (- (length inner-lst) 1)))))
                  (begin (set! v-num (+ v-num 1))
                         (letrec ([f-key (string->symbol (string-append "v" (number->string v-num)))]
                                  [lambda-exp `(lambda (,f-key))])
                           (begin (set! prefix-exp (car `(,(append prefix-exp (append inner-lst (list lambda-exp))))))
                                  f-key)))
                  inner-lst)))
        exp))

  (: wrap-lambda-exp (-> Exp Exp))
  (define (wrap-lambda-exp exp)
    (if (list? exp)
        (if (equal? '() exp)
            exp
            (if (and (list? (car exp)) (= 2 (length (car exp))) (equal? 'lambda (caar exp))) 
                (list (append (car exp) (list (wrap-lambda-exp (cdr exp)))))
                (append (list (car exp)) (cast (wrap-lambda-exp (cdr exp)) (Listof Exp)))))
        exp))
        
  (let ([base-exp (_cps (tail-prepare exp))])
    (wrap-lambda-exp (cast (append prefix-exp (if (list? base-exp) base-exp (list base-exp))) (Listof Exp)))))
     
(: lambda-body-cps (-> Exp Exp))
(define (lambda-body-cps exp)
  (match exp
    [(list 'lambda _ _)
     (lambda-cps exp)]
    [(list 'if cond-exp then-exp else-exp)
     (if (has-call? cond-exp)
         (tail-cps exp)
         `(if ,cond-exp ,(tail-cps then-exp) ,(tail-cps else-exp)))]
    [_
     (tail-cps exp)]))

(: lambda-cps (-> Exp Exp))
(define (lambda-cps exp)
  (match exp
    [(list 'lambda args body)
     `(lambda ,(append (if (list? args) args (list args)) '(K)) ,(if (and (list? body) (equal? 'lambda (car body)))
                                                                     (list 'K (lambda-body-cps body))
                                                                     (lambda-body-cps body)))]
    [_
     (if (list? exp)
         (map (lambda ([x : Exp]) (lambda-cps x)) exp)
         exp)]))


; for test
(define test-a
  '(lambda (q) (+ 22 (- x 3) x)))

(define test-b
  '(lambda (q) (+ 22 (f x) 37)))

(define test-c
  '(lambda (q) (g 22 (f x))))

(define test-d
  '(lambda (q) (+ 22 (f (g x)) ((lambda (x) (x 1)) 2)  33 (g y))))

(define test-e
  '(lambda (q) (if (zero? (m q)) (+ 1 (g x)) (f x))))


(define fact
   '(lambda (n)
      ((lambda (fact)
         ((fact fact) n))
       (lambda (fact)
         (lambda (n)
           (if (zero? n)
               1
               (* n ((fact fact) (sub1 n)))))))))


(equal? (lambda-cps fact) '(lambda (n K)
   ((lambda (fact K) (fact fact (lambda (v1) (v1 n K))))
    (lambda (fact K)
      (K
       (lambda (n K) (if (zero? n) (K 1) (fact fact (lambda (v1) (v1 (sub1 n) (lambda (v2) (K (* n v2))))))))))
    K)))