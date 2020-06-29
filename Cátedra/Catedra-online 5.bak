#lang play
(print-only-errors #t)


#|
  <expr ::= <num>
          | {+ <Expr> <Expr> }
          | {- <Expr> <Expr> }
          | {with {<id> <Expr>} <Expr> }
          | <id>
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r)
  (with bound-id named-expr body)
  (id x))

; parse :: s-expr -> Expr
(define (parse sexpr)
  (match sexpr
  [(? number?) (num sexpr)]
  [(? symbol?) (id sexpr)]
  [(list '+ l r) (add (parse l) (parse r))]
  [(list '- l r) (sub (parse l) (parse r))]
  [(list '/ l r) (div (parse l) (parse r))]
  [(list 'with (list i e) b)
   (with i (parse e) (parse b))]))

; calc : Expr -> num
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(div l r) (if (equal? r (num 0))
                   (error "no se permite dividir por 0")
                   (/ (calc l) (calc r)))]
    [(id x) (error "free identifier:" x)]
    [(with x e b) (calc (subst x (num (calc e)) b))]))

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
                         ])) 

(test (subst 'x (num 19) (num 1)) (num 1))

(test (subst 'x (num 10) (add (id 'x) (id 'x)))
      (add (num 10) (num 10)))

(test (subst 'x (num 10) (id 'x)) (num 10))
(test (subst 'x (num 19) (id 'y)) (id 'y))

(test (subst 'x (num 10) (parse '{with {x 1} x}))
      (parse '{with {x 1} x}))
; run :: sexpr-v ->number
(define (run prog)
  (calc (parse prog)))


(test (parse '{with {x 1} {+ x x}}) (with 'x (num 1) (add (id 'x) (id 'x))))

(test (parse '{with {x {with {y {+ 2 2}}
                             {+ y y}}}
                    {+ x x}})(with 'x (with 'y (add (num 2) (num 2)) (add (id 'y) (id 'y))) (add (id 'x) (id 'x))))

(test (parse 3) (num 3))

(test (parse '{+ 4 2})
      (add (num 4) (num 2)))

(test (parse '{+ 4 {- 5 1}})
      (add (num 4) (sub (num 5) (num 1))))

(test/exn (calc (div (num 10) (num 0)))
      "no se permite dividir por 0")

(test (calc (div (num 10) (num 2)))
      5)

(test (calc (add (num 10) (num 20)))
      30)

(test (calc (add (num 10) (sub (num 20) (num 10))))
      20)
(test (run '{+ 4 2})
      6)
(test (run 3) 3)

(test (run '{+ {- 4 1} 2}) 5)

(test/exn (run 'x) "free")

(test (run '{with {x {+ 5 5}}
      {+ x x}}) 20)

#|
1
1/3
{+ 1 2}
{- 1 2}

{with {x {+ 5 5}}
      {+ x x}}
-->[calc]
{with {x 10}
      {+ x x}}
--> [subst x]
{+ 10 10}
--> [calc]
20
|#