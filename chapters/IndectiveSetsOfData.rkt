#lang typed/racket

(: list-length (All (A) (-> (Listof A) Integer)))
(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))


(: nth-element (All (A) (-> (Listof A) Integer A)))
(define (nth-element lst n)
  (if (null? lst)
      (raise 'list-too-short #t) 
      (if (= n 0)
          (car lst)
          (nth-element (cdr lst) (- n 1)))))

(: remove-first (All (A) (-> A (Listof A) (Listof A))))
(define (remove-first item lst)
  (if (null? lst)
      lst
      (if (equal? item (car lst))
          (cdr lst)
          (cons (car lst) (remove-first item (cdr lst))))))

(: occurs-free? (-> Symbol (Rec Expr (Listof (U Symbol Expr))) Boolean))
(define (occurs-free? symbol expr)
 (if (null? expr)
     #f
     (if (symbol? (car expr))
         (if (equal? symbol (car expr))
             #t
             (occurs-free? symbol (cdr expr)))
         (or (occurs-free? symbol (car expr))
             (occurs-free? symbol (cdr expr))))))


(: subst (-> Symbol Symbol (Rec Expr (Listof (U Symbol Expr))) (Rec Expr (Listof (U Symbol Expr)))))
(define (subst new old expr)
  (if (null? expr)
      expr
      (if (list? (car expr))
          (cons (subst new old (car expr)) (subst new old (cdr expr)))
          (if (equal? old (car expr))
              (cons new (subst new old (cdr expr)))
              (cons (car expr) (subst new old (cdr expr)))))))


(: number-elements-from (All (A) (-> (Listof A) Integer (Listof (List Integer A)))))
(define (number-elements-from lst n)
  (if (null? lst)
      lst
      (cons (list n (car lst)) (number-elements-from (cdr lst) (+ n 1)))))

(: number-elements (All (A) (-> (Listof A) (Listof (List Integer A)))))
(define (number-elements lst)
  (number-elements-from lst 0))

(: list-sum (-> (Listof Integer) Integer))
(define (list-sum lst)
  (if (null? lst)
      0
      (+ (car lst) (list-sum (cdr lst)))))

(: partial-vector-sum (-> (Vectorof Integer) Integer Integer))
(define (partial-vector-sum vec n)
  (if (or (null? vec) (zero? n))
      (vector-ref vec n)
      (+ (vector-ref vec n) (partial-vector-sum vec (- n 1)))))

