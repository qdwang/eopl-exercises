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

(define-type Cont (U Null zero-cont if-cont diff-arg1-cont diff-arg2-cont
                     let-cont rator-cont rand-cont newref-cont deref-cont
                     setref-var-cont setref-val-cont))

(struct zero-cont ([cont : Cont]))
(struct if-cont ([then-exp : Exp] [else-exp : Exp] [env : Env] [cont : Cont]))
(struct diff-arg1-cont ([expr2 : Exp] [env : Env] [cont : Cont]))
(struct diff-arg2-cont ([expval1 : ExpVal] [cont : Cont]))
(struct let-cont ([var : Var] [body : Exp] [env : Env] [cont : Cont]))
(struct rator-cont ([rand : Exp] [env : Env] [cont : Cont]))
(struct rand-cont ([rator : ExpVal] [env : Env] [cont : Cont]))
(struct newref-cont ([cont : Cont]))
(struct deref-cont ([cont : Cont]))
(struct setref-var-cont ([val : Exp] [env : Env] [cont : Cont]))
(struct setref-val-cont ([var : ExpVal] [cont : Cont]))
                   
(: list-update (All (A) (-> (Listof A) Integer A (Listof A))))
(define (list-update lst index val)
  (if (= index 0)
      (append (list val) (cdr lst))
      (append (list (car lst)) (list-update (cdr lst) (- index 1) val))))

(: eval-program (-> Exp Env Store ExpVal))
(define (eval-program expr env store)

  (: apply-cont (-> Cont ExpVal ExpVal))
  (define (apply-cont cont expval)
    (match cont
      [(zero-cont next-cont)
       (apply-cont next-cont (if (num-val? expval)
             (if (zero? (num-val-val expval))
                 (bool-val #t)
                 (bool-val #f))
             (error "zero-argument-error" expval)))]
      
      [(if-cont then-exp else-exp env next-cont)
       (if (bool-val? expval)
             (if (bool-val-val expval)
                 (value-of then-exp env next-cont)
                 (value-of else-exp env next-cont))
             (error "if-argument-error" expval))]
      
      [(diff-arg1-cont expr2 env next-cont)
       (value-of expr2 env (diff-arg2-cont expval next-cont))]
      
      [(diff-arg2-cont expval1 next-cont)
       (apply-cont next-cont (if (and (num-val? expval1) (num-val? expval))
             (num-val (- (num-val-val expval1) (num-val-val expval)))
             (error "diff-arguments-error" expval1 expval)))]
      
      [(let-cont var body env next-cont)
       (value-of body (extend-env var expval env) next-cont)]
      
      [(rator-cont rand env next-cont)
       (value-of rand env (rand-cont expval env next-cont))]
      
      [(rand-cont rator env next-cont)
       (if (proc-val? rator)
             (value-of
              (proc-val-body rator)
              (extend-env
               (proc-val-var rator)
               expval
               env) next-cont)
             (error "call-arguments-error" rator))]
      
      [(newref-cont next-cont)
       (apply-cont next-cont (let ([store-len (length store)])
         (begin (set! store (append store (list expval)))
                (ref-val store-len))))]
      
      [(deref-cont next-cont)
       (apply-cont next-cont (if (ref-val? expval)
             (list-ref store (ref-val-index expval))
             (error "deref-argument-error" expval)))]
      
      [(setref-var-cont val env next-cont)
       (value-of val env (setref-val-cont expval next-cont))]
      
      [(setref-val-cont var next-cont)
       (apply-cont next-cont (if (ref-val? var)
             (begin (set! store (list-update store (ref-val-index var) expval))
                    var)
             (error "setref-argument-error" var expval)))]
      
      [null expval]))


  (: value-of (-> Exp Env Cont ExpVal))
  (define (value-of expr env cont)
    (match expr
      [(const-exp const) (apply-cont cont (num-val const))]
      
      [(var-exp var)
       (let ([expval (apply-env env var)])
         (if (lazy-val? expval)
                         (value-of (lazy-val-body expval) env cont)
                         (apply-cont cont expval)))]
      
      [(proc-exp var body) (apply-cont cont (proc-val var body))]
      
      [(newref-exp exp)
       (value-of exp env (newref-cont cont))]
      
      [(deref-exp exp)
       (value-of exp env (deref-cont cont))]
      
      [(setref-exp var val)
       (value-of var env (setref-var-cont val env cont))]
     
      [(lazy-exp body)
       (apply-cont cont (lazy-val body))]
      
      [(zero?-exp exp)
       (value-of exp env (zero-cont cont))]
      
      [(if-exp cond-exp then-exp else-exp)
       (value-of cond-exp env (if-cont then-exp else-exp env cont))]
      
      [(diff-exp exp1 exp2)
       (value-of exp1 env (diff-arg1-cont exp2 env cont))]
      
      [(let-exp var exp1 body)
       (value-of exp1 env (let-cont var body env cont))]
      
      [(letrec-exp p-name b-var p-body letrec-body)
       (value-of letrec-body (extend-env p-name (proc-val b-var p-body) env) cont)]
      
      [(call-exp rator rand)
       (value-of rator env (rator-cont rand env cont))]))
  
  (value-of expr env null))
           
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