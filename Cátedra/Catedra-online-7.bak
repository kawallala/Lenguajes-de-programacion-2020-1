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

; interp : Expr x List[FunDef] -> num
(define (interp expr fundefs)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l fundefs) (interp r fundefs))]
    [(sub l r) (- (interp l fundefs) (interp r fundefs))]
    [(div l r) (if (equal? r (num 0))
                   (error "no se permite dividir por 0")
                   (/ (interp l fundefs) (interp r fundefs)))]
    [(id x) (error "free identifier:" x)]
    [(with x e b) (interp (subst x (num (interp e fundefs)) b) fundefs)]
    [(app f e)
      (def (fundef _ p b) (lookup-fundef f fundefs))
      (interp (subst p (num (interp e fundefs)) b) fundefs)]))

; subst :: Sym Expr Expr -> Expr
; reemplaza todas las ocurrencias libres del identificador por
; la primera expresion en la segunda expresion
(define (subst x e expr)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst x e l) (subst x e r))]
    [(sub l r) (sub (subst x e l) (subst x e r))]
    [(id i) (if (equal? i x)
                e
                expr)]
    [(with wx we wb) (if (equal? wx x)
                         (with wx (subst x e we) wb)
                         (with wx (subst x e we) (subst x e wb)))
                         ]
    [(app f a) (app f (subst x e a))])) 
#|
(test (subst 'x (num 19) (num 1)) (num 1))

(test (subst 'x (num 10) (add (id 'x) (id 'x)))
      (add (num 10) (num 10)))

(test (subst 'x (num 10) (id 'x)) (num 10))
(test (subst 'x (num 19) (id 'y)) (id 'y))

(test (subst 'x (num 10) (parse '{with {x 1} x}))
      (parse '{with {x 1} x}))
|#

; run :: sexpr-v x List[FunDef] ->number
(define (run prog [fundefs '()])
  (interp (parse prog) fundefs))

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