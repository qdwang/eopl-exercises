#lang typed/racket

(define-type Exp (Rec exp (U Symbol Integer (Listof exp))))



(: cps-convert (-> Exp Exp))
(define (cps-convert input-exp)

  (define [v-num : Integer] 0)
    
  (: cps-s (-> Exp Exp))
  (define (cps-s input-exp)
    (match input-exp
      [x (quasiquote (cont (unquote input-exp)))]
      
      [('lambda args body)
       (quasiquote (lambda (unquote (reverse (cons 'cont args))) (unquote (cps body))))]
      
      [('if cond-exp then-exp else-exp)
       (quasiquote (if (unquote cond-exp) 
                          



; for test
(define test-exp1
'(lambda (x)
  (if (zero? x)
      (+ a (f b) 5 6)
      (f c))))

;to
;empty cont will be (lambda (x) x)
'(lambda (x cont)
  (if (zero? x)
      ((f b) (lambda (v1) (cont (+ a v1 5 6)))) 
      (f c cont)))