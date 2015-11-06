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


(struct var-exp ([var : Any]))
(struct lambda-exp ([bound-var : Symbol] [body : lc-exp]))
(struct app-exp ([rator : lc-exp] [rand : lc-exp]))

(define-type lc-exp (U var-exp lambda-exp app-exp Null))

(: occurs-free (-> Any lc-exp Boolean))
(define (occurs-free symbol exp)
  (if (null? exp)
      #f
      (match exp
        [(var-exp var) (if (equal? var symbol) #t #f)]
        [(lambda-exp bound-var body)
         (if (equal? bound-var symbol)
             #f
             (occurs-free symbol body))]
        [(app-exp rator rand)
         (or (occurs-free symbol rator)
             (occurs-free symbol rand))])))

(: parse-expression (-> (Rec exp (U Symbol Number (Listof exp))) lc-exp))
(define (parse-expression datum)
  (cond
    [(symbol? datum) (var-exp datum)]
    [(list? datum)
     (if (equal? (car datum) 'lambda)
         (lambda-exp
          (car (cast (cadr datum) (Listof Symbol)))
          (parse-expression (caddr datum)))
         (app-exp (parse-expression (car datum)) (parse-expression (cadr datum))))]
    [else null]))

         
         
    