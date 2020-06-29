#lang play
#|
<expr> ::= <number>
          | {add <expr> <expr>
          | {id <variable>}
          | {fun <expr> <expr>
          | {with {id <expr>} <expr>}
          | {app <expr> <expr>
          | {dyn-with {id <expr>} <expr>}
          | {dyn-app <expr> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (id x)
  (fun id body)
  (app fun-expr arg-expr)
  (dyn-app fun-epr arg-expr))

;interp :: <expr> env -> num
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(add l r) (numV+ (interp l env) (interp r env))]
    [(id x) (lookup-env x env)]
    [(fun id body) (closureV id body env)]
    [(app fun-expr arg-expr)
     (def (closureV id body fenv) (interp fun-expr env))
     (interp body
             (extend-env id
                         (interp arg-expr env)
                         fenv))]
    [(dyn-app fun-expr arg-expr) (interp body
                                         (extend-env id
                                                     (interp arg-expr env)
                                                     env))]))