#lang play
(print-only-errors #t)

#|
  <expr ::= <num>
          | {+ <Expr> <Expr> }
          | {- <Expr> <Expr> }
          | {with {<id> <Expr>} <Expr> }
          | <id>
          | {<id> <Expr>} ; function aplication
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r)
  (with bound-id named-expr body)
  (id x)
  (app fun-name arg-expr))

; function definitions
(deftype FunDef
  (fundef name arg body))

; lookup-fundef :: Sym List[FunDef] -> FunDef (o error)
(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function:" f)]
    [(cons fd fds) (if (equal? f (fundef-name fd))
                   fd
                   (lookup-fundef f fds))]))
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
   (with i (parse e) (parse b))]
  [(list f e) (app f (parse e))]))

; convert:: Expr -> Expr
#|
(define (convert expr) (let [level 0])
  (match expr
    [(num n) n]
    [(add l r) (+ (convert l) (convert r))]
    [(sub l r) (- (convert l) (convert r))]
    [(with x e b) (]
|#

; interp : Expr x Env x List[FunDef] -> num
(define (interp expr env fundefs)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l env fundefs) (interp r env fundefs))]
    [(sub l r) (- (interp l env fundefs) (interp r env fundefs))]
    [(div l r) (if (equal? r (num 0))
                   (error "no se permite dividir por 0")
                   (/ (interp l env fundefs) (interp r env fundefs)))]
    [(id x) (lookup-env x env)]
    [(with x e b)
     (interp b (extend-env x (interp e env fundefs) env) fundefs)]
    [(app f e)
      (def (fundef _ p b) (lookup-fundef f fundefs))
      (interp b (extend-env p (interp e env fundefs)
                            (empty-env) ;static scope
                            ;if use 'env' then dynamic scope
                            ) fundefs)]))

; run :: sexpr-v x List[FunDef] ->number
(define (run prog [fundefs '()])
  (interp (parse prog) (empty-env) fundefs))

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

(test (run '{f 2} (list (fundef 'f 'x (parse '{+ x x}))))
      4)

(test (run '{g {f 2}} (list (fundef 'f 'x (parse '{+ x x}))
      (fundef 'g 'y (parse '{+ y 1}))))
      5)

(test/exn (run '{f 10}
               (list (fundef 'f 'n (parse '{n n}))))
          "undefined function")
;exercise: add ifz to the language, and define recursive functions
;option 1 (same namespace)
;---[subst]--->{10 10}--> error: 10 is not a function
;option 2 (separate namespaces)
;---[subst]--->{n 10} ---> error: undefined funciton n

;scode estatico: region de texto (del body)
;scope dinamico: region de computo (del body)

; we want static scope
(test/exn
 (run '{with {n 10}
             {f 2}}
      (list (fundef 'f 'x (id 'n))))
 "free identifier")