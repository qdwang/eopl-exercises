#lang typed/racket

(define-type Env (Rec env (U Null (List Symbol ExpVal env))))

(: empty-env (-> Env))
(define (empty-env) null)

(: extend-env (-> Symbol ExpVal Env Env))
(define (extend-env symbol val env)
  (list symbol val env))

(: apply-env (-> Env Symbol ExpVal))
(define (apply-env env symbol)
  (if (null? env)
      null
      (if (equal? (car env) symbol)
          (cadr env)
          (apply-env (cast (list-ref env 2) Env) symbol))))


(define-type ExpVal (U num-val bool-val Null))

(struct num-val ([val : Integer]))
(struct bool-val ([val : Boolean]))


(define-type Exp (U const-exp zero?-exp if-exp diff-exp var-exp let-exp))
(define-type Var Symbol)

(struct const-exp ([const : Integer]))
(struct zero?-exp ([expr : Exp]))
(struct if-exp ([cond : Exp] [then-exp : Exp] [else-exp : Exp]))
(struct diff-exp ([expr1 : Exp] [expr2 : Exp]))
(struct var-exp ([var : Var]))
(struct let-exp ([var : Var] [expr1 : Exp] [expr2 : Exp]))


(: expval->num (-> ExpVal Integer))
(define (expval->num expval)
  (cond
    [(num-val? expval) (num-val-val expval)]
    [else (error "expval-num-error" expval)]))
         
(: expval->bool (-> ExpVal Boolean))
(define (expval->bool expval)
  (cond
    [(bool-val? expval) (bool-val-val expval)]
    [else (error "expval-bool-error" expval)]))

(: value-of (-> Exp Env ExpVal))
(define (value-of expr env)
  (match expr
    [(const-exp const) (num-val const)]
    [(var-exp var) (apply-env env var)]
    [(zero?-exp exp)
     (let ([inner-exp-val (value-of exp env)])
       (if (num-val? inner-exp-val)
         (if (zero? (expval->num inner-exp-val))
             (bool-val #t)
             (bool-val #f))
         (error "zero-argument-error" inner-exp-val)))]
    [(if-exp cond-exp then-exp else-exp)
     (let ([inner-exp-val (value-of cond-exp env)])
       (if (bool-val? inner-exp-val)
          (if (expval->bool inner-exp-val)
              (value-of then-exp env)
              (value-of else-exp env))
          (error "if-argument-error" inner-exp-val)))]
    [(diff-exp exp1 exp2)
     (let ([inner-exp1-val (value-of exp1 env)]
           [inner-exp2-val (value-of exp2 env)])
       (if (and (num-val? inner-exp1-val) (num-val? inner-exp2-val))
          (num-val (- (expval->num inner-exp1-val) (expval->num inner-exp2-val)))
          (error "diff-arguments-error" inner-exp1-val inner-exp2-val)))]
    [(let-exp var exp1 body)
     (value-of body (extend-env var (value-of exp1 env) env))]))
     
; for test
'(expval->num
 (value-of
  (let-exp
   'a
   (const-exp 4)
   (if-exp (zero?-exp (diff-exp (var-exp 'a) (const-exp 4)))
           (const-exp 3)
           (const-exp 4)))
  (empty-env)))

