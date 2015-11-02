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

