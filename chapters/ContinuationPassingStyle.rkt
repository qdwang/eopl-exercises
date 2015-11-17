#lang typed/racket

; for test
(: f (-> Integer Integer))
(define (f x) 1)
(: g (-> Integer Integer Integer))
(define (g x z) 1)
(define y 2)
(: h (-> Integer Integer Integer Integer))
(define (h a b c) 1)
(define (test-exp)
  (lambda ([x : Integer])
    (cond
      ((zero? x) 17)
      ((= x 1) (f (- x 13)))
      ((= x 2) (+ 22 (- x 3) x))
      ((= x 3) (+ 22 (f x) 37))
      ((= x 4) (g 22 (f x)))
      ((= x 5) (+ 22 (f x) 33 (g y 2)))
      (else (h (f x) (- 44 y) (g y 3))))))