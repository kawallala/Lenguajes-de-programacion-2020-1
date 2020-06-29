#lang play
(print-only-errors #t)


#|
  <expr ::= (num <n>)
          | (add <expr> <expr>)
          | (sub <expr> <expr>)
          | (with <id> <expr> <expr>)
          | (id <sym>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r)
  (with bound-id named-expr body)
  (id x))

#|
   <sexpr-v> ::= <n>
               | '(+ <sexpr-v> <sexpr-v>)
               | '(- <sexpr-v> <sexpr-v>)
               | '(with (<id> <sexpr-v>) (<sexpr-v>))
               | <sym>
|#
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

(test (parse '{with {x 1} {+ x x}}) (with 'x (num 1) (add (id 'x) (id 'x))))

(test (parse '{with {x {with {y {+ 2 2}}
                             {+ y y}}}
                    {+ x x}})(with 'x (with 'y (add (num 2) (num 2)) (add (id 'y) (id 'y))) (add (id 'x) (id 'x))))

(test (parse 3) (num 3))

(test (parse '{+ 4 2})
      (add (num 4) (num 2)))

(test (parse '{+ 4 {- 5 1}})
      (add (num 4) (sub (num 5) (num 1))))


; calc : Expr -> num
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(div l r) (if (equal? r (num 0))
                   (error "no se permite dividir por 0")
                   (/ (calc l) (calc r)))]
    [(id x) (error "undefined identifier:" x)]
    [(with x e b) (calc (subst x (calc e) b))]))

; subst :: Sym Expr Expr -> Expr
; substituye el identificador por la primera expresion en la segunda expresion
(define (subst x e body)
  body) ; FIX

(test (subst 'x (num 10) (add (id 'x) (id 'x)))
      (add (num 10) (num 10)))

; run :: sexpr-v ->number
(define (run prog)
  (calc (parse prog)))

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

(test/exn (run 'x) "undefined")

(test (run '{with {x {+ 5 5}}
      {+ x x}}) 10)

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