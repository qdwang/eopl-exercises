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

(define-type Store (Listof ExpVal))
                    
(define-type ExpVal (U num-val bool-val proc-val ref-val lazy-val Null))

; expressed value
(struct num-val ([val : Integer]))
(struct bool-val ([val : Boolean]))
(struct proc-val ([var : Var] [body : Exp]))
(struct ref-val ([index : Integer]))
(struct lazy-val ([body : Exp]))
                 
(define-type Exp
  (U const-exp zero?-exp if-exp diff-exp
     var-exp let-exp proc-exp call-exp
     letrec-exp newref-exp deref-exp setref-exp
     lazy-exp))

(define-type Var Symbol)

(struct const-exp ([const : Integer]))
(struct zero?-exp ([expr : Exp]))
(struct if-exp ([cond : Exp] [then-exp : Exp] [else-exp : Exp]))
(struct diff-exp ([expr1 : Exp] [expr2 : Exp]))
(struct var-exp ([var : Var]))
(struct let-exp ([var : Var] [expr : Exp] [body : Exp]))
(struct proc-exp ([var : Var] [body : Exp]))
(struct call-exp ([rator : Exp] [rand : Exp]))
(struct letrec-exp
  ([p-name : Var] [b-var : Var] [p-body : Exp] [letrec-body : Exp]))
(struct newref-exp ([val : Exp]))
(struct deref-exp ([var : Exp]))
(struct setref-exp ([var : Exp] [val : Exp]))
(struct lazy-exp ([exp : Exp]))

(: list-update (All (A) (-> (Listof A) Integer A (Listof A))))
(define (list-update lst index val)
  (if (= index 0)
      (append (list val) (cdr lst))
      (append (list (car lst)) (list-update (cdr lst) (- index 1) val))))

(: eval-program (-> Exp Env Store ExpVal))
(define (eval-program expr env store)
  
  (: lazy->value (-> ExpVal Env ExpVal))
  (define (lazy->value expval env)
    (if (lazy-val? expval)
        (value-of (lazy-val-body expval) env)
        expval))
  
  (: value-of (-> Exp Env ExpVal))
  (define (value-of expr env)
    (match expr
      [(const-exp const) (num-val const)]
      [(var-exp var) (lazy->value (apply-env env var) env)]
      [(proc-exp var body) (proc-val var body)]
      
      [(newref-exp exp)
       (let ([inner-exp-val (value-of exp env)]
             [store-len (length store)])
         (begin (set! store (append store (list inner-exp-val)))
                (ref-val store-len)))]
      
      [(deref-exp exp)
       (let ([var (value-of exp env)])
         (if (ref-val? var)
             (list-ref store (ref-val-index var))
             (error "deref-argument-error" var)))]
      
      [(setref-exp var val)
       (let ([var (value-of var env)]
             [val (value-of val env)])
         (if (ref-val? var)
             (begin (set! store (list-update store (ref-val-index var) val))
                    var)
             (error "setref-argument-error" var val)))]

      [(lazy-exp body)
       (lazy-val body)]
      
      [(zero?-exp exp)
       (let ([inner-exp-val (value-of exp env)])
         (if (num-val? inner-exp-val)
             (if (zero? (num-val-val inner-exp-val))
                 (bool-val #t)
                 (bool-val #f))
             (error "zero-argument-error" inner-exp-val env)))]
      
      [(if-exp cond-exp then-exp else-exp)
       (let ([inner-exp-val (value-of cond-exp env)])
         (if (bool-val? inner-exp-val)
             (if (bool-val-val inner-exp-val)
                 (value-of then-exp env)
                 (value-of else-exp env))
             (error "if-argument-error" inner-exp-val)))]
      
      [(diff-exp exp1 exp2)
       (let ([inner-exp1-val (value-of exp1 env)]
             [inner-exp2-val (value-of exp2 env)])
         (if (and (num-val? inner-exp1-val) (num-val? inner-exp2-val))
             (num-val (- (num-val-val inner-exp1-val) (num-val-val inner-exp2-val)))
             (error "diff-arguments-error" inner-exp1-val inner-exp2-val)))]
      
      [(let-exp var exp1 body)
       (value-of body (extend-env var (value-of exp1 env) env))]
      
      [(letrec-exp p-name b-var p-body letrec-body)
       (value-of letrec-body (extend-env p-name (proc-val b-var p-body) env))]
      
      [(call-exp rator rand)
       (let ([proc (value-of rator env)])
         (if (proc-val? proc)
             (value-of
              (proc-val-body proc)
              (extend-env
               (proc-val-var proc)
               (value-of rand env)
               env))
             (error "call-arguments-error" rator)))]))
  
  (value-of expr env))
           
(define test-store '())

; for test 1
(num-val-val
 (cast (eval-program
  (let-exp
   'a
   (const-exp 4)
   (if-exp (zero?-exp (diff-exp (var-exp 'a) (const-exp 4)))
           (const-exp 0)
           (const-exp 1)))
  (empty-env) test-store) num-val))

; for test 2
(num-val-val
 (cast (eval-program
  (let-exp
   'a
   (proc-exp 'x
             (if-exp (zero?-exp (var-exp 'x))
                     (const-exp 0)
                     (diff-exp (var-exp 'x) (const-exp 1))))
   (if-exp (zero?-exp (diff-exp (call-exp (var-exp 'a) (const-exp 5)) (const-exp 4)))
           (const-exp 0)
           (const-exp 1)))
  (empty-env) test-store) num-val))

; for test 3
(num-val-val
 (cast (eval-program
  (letrec-exp
   'foo
   'x
   (if-exp (zero?-exp (var-exp 'x))
           (const-exp 0)
           (call-exp (var-exp 'foo) (diff-exp (var-exp 'x) (const-exp 1))))
   (if-exp (zero?-exp (call-exp (var-exp 'foo) (const-exp 5)))
           (const-exp 0)
           (const-exp 1)))
  (empty-env) test-store) num-val))

; for test 4
(num-val-val
 (cast (eval-program
  (let-exp 'a (const-exp 5)
   (diff-exp (let-exp 'a (const-exp 10) (var-exp 'a)) (var-exp 'a)))
  (empty-env) test-store) num-val))

; for test 5
(num-val-val
 (cast (eval-program
  (let-exp 'a (newref-exp (const-exp 998))
   (diff-exp (let-exp 'b (setref-exp (var-exp 'a) (const-exp 10)) (const-exp 10))
             (deref-exp (var-exp 'a))))
  (empty-env) test-store) num-val))

; for test 6
(num-val-val
 (cast (eval-program
  (letrec-exp 'loop 'x (call-exp (var-exp 'loop) (var-exp 'x))
              (let-exp 'f (proc-exp 'z (const-exp 11))
                       (call-exp (var-exp 'f) (lazy-exp (call-exp (var-exp 'loop) (const-exp 0))))))
  (empty-env) test-store) num-val))

; for test 7
(num-val-val
 (cast (eval-program
  (letrec-exp 'loop 'x (const-exp 11)
              (let-exp 'f (proc-exp 'z (diff-exp (const-exp 11) (var-exp 'z)))
                       (call-exp (var-exp 'f) (lazy-exp (call-exp (var-exp 'loop) (const-exp 0))))))
  (empty-env) test-store) num-val))