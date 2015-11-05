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


(struct var-exp ([var : Symbol]))
(struct lambda-exp ([bound-var : Symbol] [body : (U Null lc-exp)]))
(struct app-exp ([rator : (U Null lc-exp)] [rand : (U Null lc-exp)]))
(struct lc-exp ([var-exp : var-exp] [lambda-exp : lambda-exp] [app-exp : app-exp]))
  