#lang play
(print-only-errors #t)

#|
  <expr ::= <num>
          | {+ <Expr> <Expr> }
          | {- <Expr> <Expr> }
          | {with {<id> <Expr>} <Expr> } ; azucar siontactico
          | <id>
          | {<Expr> <Expr>} ; function aplication
          | {fun {<id>} <Expr>} ;function definition
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r)
  (id x)
  (app fun-expr arg-expr)
  (fun id body))

; ambientes: Env
; tipo de dato abstracto
; (= name + operation)
; Env
; empty-env :: Env
; extend-env :: Id x Val x Env -> env
; lookup-env :: Id x Env -> Val (o error)
(deftype Env
  (mtEnv)
  (aEnv id val next))
  
;(define (empty-env) (mtEnv))
(define empty-env mtEnv)
(define extend-env aEnv)
(define (lookup-env id env)
  (match env
    [(mtEnv) (error "free identifier:" id)]
    [(aEnv x v n) (if (equal? x id)
                      v
                      (lookup-env id n))]))
; parse :: s-expr -> Expr
(define (parse sexpr)
  (match sexpr
  [(? number?) (num sexpr)]
  [(? symbol?) (id sexpr)]
  [(list '+ l r) (add (parse l) (parse r))]
  [(list '- l r) (sub (parse l) (parse r))]
  [(list '/ l r) (div (parse l) (parse r))]
  [(list 'with (list i e) b)
   ;rewrite as (app (fun ...) ...)
   (app (fun i (parse b)) (parse e))]
  [(list 'fun (list x) b) (fun x (parse b))]
  [(list f e) (app (parse f) (parse e))]))

; convert:: Expr -> Expr
#|
(define (convert expr) (let [level 0])
  (match expr
    [(num n) n]
    [(add l r) (+ (convert l) (convert r))]
    [(sub l r) (- (convert l) (convert r))]
    [(with x e b) (]
|#

(deftype Val
  (numV n)
  (closureV x b e)
  (exprV expr env))

; interp : Expr x Env  -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun x b) (closureV x b env)] ; capture current enviroment
    [(add l r) (numV+ (strict (interp l env)) (strict(interp r env)))]
    [(sub l r) (numV- (strict (interp l env)) (strict(interp r env)))]
    [(id x) (lookup-env x env)]
    [(app f e)
      (def (closureV p b fenv) (strict(interp f env)))
      (interp b
              (extend-env p
                          ;(interp e env); eager evaluaton
                          (exprV e env)
                           fenv))])) ;restore definition-time enviroment
(define (numV+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (numV- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

; strict :: Val -> Val (except an exprV)
;strict :: numV o closureV o exprV -> numV o closureV
(define (strict v)
  (match v
    [(numV n) v]
    [(closureV x b e) v]
    [(exprV expr env) (strict(interp expr env))]))

; run :: sexpr-v ->number
(define (run prog)
  (match (strict (interp (parse prog) (empty-env)))
    [(numV n) n]
    [(closureV x p e) (closureV x p e)]))

#|
(test (parse '{with {x 1} {+ x x}}) (with 'x (num 1) (add (id 'x) (id 'x))))

(test (parse '{with {x {with {y {+ 2 2}}
                             {+ y y}}}
                    {+ x x}})(with 'x (with 'y (add (num 2) (num 2)) (add (id 'y) (id 'y))) (add (id 'x) (id 'x))))

(test (parse 3) (num 3))

(test (parse '{+ 4 2})
      (add (num 4) (num 2)))

(test (parse '{+ 4 {- 5 1}})
      (add (num 4) (sub (num 5) (num 1))))

(test/exn (interp (div (num 10) (num 0)))
      "no se permite dividir por 0")

(test (interp (div (num 10) (num 2)))
      5)

(test (interp (add (num 10) (num 20)))
      30)

(test (interp (add (num 10) (sub (num 20) (num 10))))
      20)
|#
(test (run '{+ 4 2})
      6)
(test (run 3) 3)

(test (run '{+ {- 4 1} 2}) 5)

(test/exn (run 'x) "free")

(test/exn (run '{with {x x} x}) "free")

(test (run '{with {x {+ 5 5}}
      {+ x x}}) 20)
#|
(test (run '{f 2} (list (fundef 'f 'x (parse '{+ x x}))))
      4)

(test (run '{g {f 2}} (list (fundef 'f 'x (parse '{+ x x}))
      (fundef 'g 'y (parse '{+ y 1}))))
      5)

(test/exn (run '{f 10}
               (list (fundef 'f 'n (parse '{n n}))))
          "undefined function")
|#

;exercise: add ifz to the language, and define recursive functions
;option 1 (same namespace)
;---[subst]--->{10 10}--> error: 10 is not a function
;option 2 (separate namespaces)
;---[subst]--->{n 10} ---> error: undefined funciton n

;scode estatico: region de texto (del body)
;scope dinamico: region de computo (del body)

;testing first-class functions
(test (run '{fun {x} x}) (closureV 'x (id 'x) (mtEnv)))
(test (run '{{fun {x} x} 1}) 1)
(test (run '{with {double {fun {x} {+ x x}}}
                  {+ 1
                     {double 10}}}) 21)

(test (run '{with {double {fun {x} {+ x x}}}
                  {with {id {fun {x} x}}
                        {+ 1
                           {id {double 10}}}}}) 21)

(test (run '{with {addn {fun {n}
                             {fun {m}
                                  {+ n m}}}}
                        {{addn 10} 2}})
      12)
;now we want lazy evaluation
(test (run '{with {x y} 3}) 3)

;lazy evaluation with correct lexical scope
(test (run '{with {x {+ 4 5}}
                  {with {y {+ x x}}
                        {with {x 2}
                              y}}})
      18)