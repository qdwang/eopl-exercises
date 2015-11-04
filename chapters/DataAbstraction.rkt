#lang typed/racket

(define-type Env (Rec env (U Null (List Symbol Any env))))

(: empty-env (-> Env))
(define (empty-env) null)

(: extend-env (-> Symbol Any Env Env))
(define (extend-env symbol val env)
  (list symbol val env))

(: apply-env (-> Env Symbol Any))
(define (apply-env env symbol)
  (if (null? env)
      null
      (if (equal? (car env) symbol)
          (cadr env)
          (apply-env (cast (list-ref env 2) Env) symbol))))