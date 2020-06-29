#lang play

(print-only-errors #t)

; Ejercicio 1
(module Ejercicio1 play

(print-only-errors #t)

#|
<Expr> ::= <num>
| {<binop> <Expr> <Expr>}
| {with {<id> <Expr>} <Expr>}
| <id>
|#
(deftype Expr
  (num n)
  (binop op l r)
  (print expr)
  (id x)
  (with-eager bound-id named-expr body)
  (with-lazy bound-id named-expr body))

; binops :: list Sym
; list of valid arithmetic operations
(define binops (list '+ '- '*))

; binops? :: Any -> Boolean
; return true if its argument is a valid arithmetic operation
(define (binops? op) (member op binops))

; parse :: sexpr-v -> Expr
(define (parse sexpr)
(match sexpr
  [(? number?) (num sexpr)]
  [(? symbol?) (id sexpr)]
  [(list 'print e) (print (parse e))]
  [(list (? binops? op) l r) (binop op (parse l) (parse r))]
  [(list 'with-eager (list i e) b)
   (with-eager i (parse e) (parse b))]
  [(list 'with-lazy (list i e) b)
    (with-lazy i (parse e) (parse b))]))

; interp-binop :: Sym -> (Num Num -> Num)
(define (interp-binop op)
  (match op
    ['+ +]
    ['- -]
    ['* *]))

; interp :: Expr -> number (o error)
(define (interp expr)
  (match expr
    [(num n) n]
    [(binop op l r) ((interp-binop op) (interp l) (interp r))]
    [(print e)
     (def x (interp e))
     (printf (number->string x))
     x]
    [(id x) (error "free identifier:" x)]
    [(with-eager x e b) (interp (subst x (num (interp e)) b))]
    [(with-lazy x e b) (interp (subst x e b))]))

; subst :: Sym Expr Expr -> Expr
; reemplaza todas las ocurrencias libres del identificador
; por la primera expresión en la segunda expr
(define (subst x e expr)
  (match expr
    [(num n) expr]
    [(binop op l r) (binop op (subst x e l) (subst x e r))]
    [(print expr) (print (subst x e expr))]
    [(id i) (if (equal? i x) e expr)]
    [(with-eager wx we wb)
     (if (equal? wx x)
         (with-eager wx (subst x e we) wb) ; don't subst in nested body!
         (with-eager wx (subst x e we) (subst x e wb)))]
    [(with-lazy wx we wb)
    (if (equal? wx x)
        (with-lazy wx (subst x e we) wb) ; don't subst in nested body!
        (with-lazy wx (subst x e we) (subst x e wb)))]))

(test (subst 'x (num 10) (num 1)) (num 1))
(test (subst 'x (num 10) (binop '+ (id 'x) (id 'x)))
      (binop '+ (num 10) (num 10)))
(test (subst 'x (num 10) (id 'x)) (num 10))
(test (subst 'x (num 10) (id 'y)) (id 'y))

(test (subst 'x (num 10) (parse '{with-eager {x 1} x}))
      (parse '{with-eager {x 1} x}))

(test (subst 'x (num 10) (parse '{with-eager {y {+ x 1}} {+ x y}}))
      (parse '{with-eager {y {+ 10 1}} {+ 10 y}}))

; run :: s-expr -> Num
(define (run prog)
  (interp (parse prog)))

(test (run '{+ 4 2}) 6)
(test (run 3) 3)
(test (run '{+ {- 4 1} 2}) 5)
(test (run '{+ 1 {- 10 2/3}}) (+ 10 1/3))
(test/exn (run 'x) "free identifier")

(test (run '{with-eager {x {+ 5 5}}
                  {+ x x}})
      20)
(test (run '{with-eager {x {- 10 5}}
                  {+ x x}})
      10)
(test (run '{with-eager {x {+ 5 5}}
                  {with-eager {y {- x 3}}
                        {+ y y}}})
      14)
(test (run '{with-eager {x 1}
                  {with-eager {x x}
                        x}})
      1)

(test/exn (run '{with-eager {x x} x}) "free")

; we want eager evaluation
(test/exn (run '{with-eager {x y} 3}) "free")



;c)

;; (test (run '{print 42}) 42)

(run '{with-lazy {x {print 42}} {+ x x}})
;; ---[subst x (print 42)]--->
;; {+ {print 42} {print 42}}
;; ---[interp]----> 42 42
;; 84

(run '{with-eager {x {print 42}} {+ x x}})

(run '{with-eager {x y} 42})
(run '{with-lazy {x y} 42})
)




#|
<Expr> ::= <num>
| {<binop> <Expr> <Expr>}
| {with {<id> <Expr>} <Expr>}
| <id>
|#
(deftype Expr
  (num n)
  (binop op l r)
  (id x)
  (with bound-id named-expr body))

; binops :: list Sym
; list of valid arithmetic operations
(define binops (list '+ '- '*))

; binops? :: Any -> Boolean
; return true if its argument is a valid arithmetic operation
(define (binops? op) (member op binops))

; parse :: sexpr-v -> Expr
(define (parse sexpr)
(match sexpr
  [(? number?) (num sexpr)]
  [(? symbol?) (id sexpr)]
  [(list (? binops? op) l r) (binop op (parse l) (parse r))]
  [(list 'with (list i e) b)
    (with i (parse e) (parse b))]))

; interp-binop :: Sym -> (Num Num -> Num)
(define (interp-binop op)
  (match op
    ['+ +]
    ['- -]
    ['* *]))

; interp :: Expr -> number (o error)
(define (interp expr)
  (match expr
    [(num n) n]
    [(binop op l r) ((interp-binop op) (interp l) (interp r))]
    [(id x) (error "free identifier:" x)]
    [(with x e b) (interp (subst x (num (interp e)) b))]))

; subst :: Sym Expr Expr -> Expr
; reemplaza todas las ocurrencias libres del identificador
; por la primera expresión en la segunda expr
(define (subst x e expr)
  (match expr
    [(num n) expr]
    [(binop op l r) (binop op (subst x e l) (subst x e r))]
    [(id i) (if (equal? i x) e expr)]
    [(with wx we wb)
    (if (equal? wx x)
        (with wx (subst x e we) wb) ; don't subst in nested body!
        (with wx (subst x e we) (subst x e wb)))]))

(test (subst 'x (num 10) (num 1)) (num 1))
(test (subst 'x (num 10) (binop '+ (id 'x) (id 'x)))
      (binop '+ (num 10) (num 10)))
(test (subst 'x (num 10) (id 'x)) (num 10))
(test (subst 'x (num 10) (id 'y)) (id 'y))

(test (subst 'x (num 10) (parse '{with {x 1} x}))
      (parse '{with {x 1} x}))

(test (subst 'x (num 10) (parse '{with {y {+ x 1}} {+ x y}}))
      (parse '{with {y {+ 10 1}} {+ 10 y}}))

; run :: s-expr -> Num
(define (run prog)
  (interp (parse prog)))


; Ejercicio 2


; free-vars :: Expr -> List[Sym]
(define (free-vars expr [bound '()])
  (match expr
    [(num n) '()]
    [(binop _ l r) (append (free-vars l bound) (free-vars r bound))]
    [(id x) (if (member x bound) '() (list x))]
    [(with x e b)
     (append (free-vars e bound)
             (free-vars b (cons x bound)))]))


(test (free-vars (parse '{+ 32 10})) '())

(test (free-vars (parse '{+ x {- 10 y}})) '(x y))

(test (free-vars (parse '{with {x 3} {+ x {- 10 y}}})) '(y))

(test (free-vars (parse '{with {x 2} {with {x 3} {+ x x}}})) '())



;Ejercicio 3

;a)

#|
<Ln-Expr> ::= <num>
| {ln-<binop> <Ln-Expr> <Ln-Expr>}
| {ln-with <Expr> <Expr>}
| {free-id <id>}
| {bound-id <num>}

Una expresion es "locally nameless" si todas los indices de DeBruijn tienen un ln-with que correponde.

|#
(deftype ln-Expr
  (ln-num n)
  (ln-binop op l r)
  (free-id x)
  (bound-id num)
  (ln-with named-expr body))


; position :: List[Any] Any -> Num o #f
; retorna la posición del segundo argumento en la lista of #f si no se encuentra
(define (position l x [n 0])
  (match l
    ['() #f]
    [(list-rest y l) (if (equal? x y) n (position l x (add1 n)))]))

; convert :: Expr -> ln-Expr
(define (convert expr [bound '()])
  (match expr
    [(num n) (ln-num n)]
    [(binop op l r) (ln-binop op (convert l bound) (convert r bound))]
    [(id x)  (def i (position bound x))
             (if i (bound-id i) (free-id x)) ]
    [(with x e b) (ln-with (convert e bound) (convert b (cons x bound))) ]))


(test (convert (parse '{+ 42 x})) (ln-binop '+ (ln-num 42) (free-id 'x)))

(test (convert (parse '{with {x 42} {+ x x}}))
      (ln-with (ln-num 42)
               (ln-binop '+
                         (bound-id 0)
                         (bound-id 0)) ))

(test (convert (parse '{with {x 2}
                      {with {y 3}
                            {with {z 5}
                                  {+ y {+ z x}}}}}))
      (ln-with (ln-num 2)
               (ln-with (ln-num 3)
                        (ln-with (ln-num 5)
                                 (ln-binop '+ (bound-id 1)
                                           (ln-binop '+
                                                     (bound-id 0)
                                                     (bound-id 2)))))))


;b)

; ln-subst :: ln-Expr ln-Epxr -> ln-Expr
(define (ln-subst e expr [k 0])
  (match expr
    [(ln-num _) expr]
    [(ln-binop op l r) (ln-binop op (ln-subst e l k) (ln-subst e r k))]
    [(bound-id n) (if (equal? k n) e
                      (if (< k n) (bound-id (sub1 n)) expr))]
    [(free-id _) expr]
    [(ln-with t b) (ln-with (ln-subst e t k) (ln-subst e b (add1 k)))]))

; ln-parse : s-expr -> ln-expr
(define (ln-parse sexp) (convert (parse sexp)))

(test (ln-subst (ln-num 10) (ln-num 1)) (ln-num 1))
(test (ln-subst (ln-num 10) (ln-binop '+ (bound-id 0) (bound-id 0)))
      (ln-binop '+ (ln-num 10) (ln-num 10)))
(test (ln-subst (ln-num 10) (bound-id 0)) (ln-num 10))
(test (ln-subst (ln-num 10) (free-id 'y)) (free-id 'y))

(test (ln-subst (ln-num 10) (ln-parse '{with {x 1} x}))
      (ln-parse '{with {x 1} x}))

(test (ln-subst (ln-num 10) (ln-parse '{with {y {+ x 1}} {+ x y}}))
      (ln-parse '{with {y {+ x 1}} {+ x y}}))

; eval :: ln-Expr -> Num o error
(define (ln-interp expr)
  (match expr
    [(ln-num n) n]
    [(ln-binop op l r) ((interp-binop op) (ln-interp l) (ln-interp r))]
    [(free-id x) (error (string-append "free identifier " (symbol->string x)))]
    [(ln-with e b) (ln-interp (ln-subst (ln-num (ln-interp e)) b))]))


; ln-run :: s-expr -> Num o error
(define (ln-run sexpr) (ln-interp (ln-parse sexpr)))

(test (ln-run '{+ 4 2}) 6)
(test (ln-run 3) 3)
(test (ln-run '{+ {- 4 1} 2}) 5)
(test (ln-run '{+ 1 {- 10 2/3}}) (+ 10 1/3))
(test/exn (ln-run 'x) "free identifier")

(test (ln-run '{with {x {+ 5 5}}
                  {+ x x}})
      20)
(test (ln-run '{with {x {- 10 5}}
                  {+ x x}})
      10)
(test (ln-run '{with {x {+ 5 5}}
                  {with {y {- x 3}}
                        {+ y y}}})
      14)
(test (ln-run '{with {x 1}
                  {with {x x}
                        x}})
      1)

(test/exn (ln-run '{with {x x} x}) "free")

; we want eager evaluation
(test/exn (run '{with {x y} 3}) "free")

; Ejercicio 4

; obfuscate :: Expr -> Expr

(define (obfuscate expr)
  (match expr
    [(num n) expr]
    [(binop op l r) (binop op (obfuscate l) (obfuscate r))]
    [(id x) expr]
    [(with x e b) (def y (gensym))
                  (with y e (subst x (id y) (obfuscate b)))]))

;Ejercicio 5

(module Ejercicio5 play

(print-only-errors #t)

#|
<Expr> ::= <num>
| {<binop> <Expr> <Expr>}
| {with {<id> <Expr>} <Expr>}
| <id>
| {<id> <Expr>} ; function application
|#
(deftype Expr
  (num n)
  (binop op l r)
  (ifz n true-br false-br)
  (id x)
  (with bound-id named-expr body)
  (app fun-name arg-expr))

; binops :: list Sym
; list of valid arithmetic operations
(define binops (list '+ '- '*))

; binops? :: Any -> Boolean
; return true if its argument is a valid arithmetic operation
(define (binops? op) (member op binops))

; parse :: sexpr-v -> Expr
(define (parse sexpr)
(match sexpr
  [(? number?) (num sexpr)]
  [(? symbol?) (id sexpr)]
  [(list (? binops? op) l r) (binop op (parse l) (parse r))]
  [(list 'ifz n l r) (ifz (parse n) (parse l) (parse r))]
  [(list 'with (list i e) b)
    (with i (parse e) (parse b))]
  [(list f e) (app f (parse e))]))


; function definitions
(deftype FunDef
  (fundef name param body))

; lookup-fundef :: Sym List[FunDef] -> FunDef (o error)
(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function:" f)]
    [(cons fd fds) (if (equal? f (fundef-name fd))
                        fd
                        (lookup-fundef f fds))]))


; interp-binop :: Sym -> (Num Num -> Num)
(define (interp-binop op)
  (match op
    ['+ +]
    ['- -]
    ['* *]))

; interp :: Expr List[FunDef] -> Num (o error)
(define (interp expr fundefs)
  (match expr
    [(num n) n]
    [(binop op l r) ((interp-binop op) (interp l fundefs) (interp r fundefs))]
    [(ifz n l r) (if (zero? (interp n fundefs))
                     (interp l fundefs)
                     (interp r fundefs))]
    [(id x) (error "free identifier:" x)]
    [(with x e b) (interp (subst x (num (interp e fundefs)) b) fundefs)]
    [(app f e)
      (def (fundef _ p b) (lookup-fundef f fundefs))
      (interp (subst p (num (interp e fundefs)) b) fundefs)]))

; subst :: Sym Expr Expr -> Expr
; reemplaza todas las ocurrencias libres del identificador
; por la primera expresión en la segunda expr
(define (subst x e expr)
  (match expr
    [(num n) expr]
    [(binop op l r) (binop op (subst x e l) (subst x e r))]
    [(ifz n l r) (ifz (subst x e n) (subst x e l) (subst x e r))]
    [(id i) (if (equal? i x) e expr)]
    [(with wx we wb)
      (if (equal? wx x)
          (with wx (subst x e we) wb) ; don't subst in nested body!
          (with wx (subst x e we) (subst x e wb)))]
    [(app f a) (app f (subst x e a))]))

(test (subst 'x (num 10) (num 1)) (num 1))
(test (subst 'x (num 10) (binop '+ (id 'x) (id 'x)))
      (binop '+ (num 10) (num 10)))
(test (subst 'x (num 10) (id 'x)) (num 10))
(test (subst 'x (num 10) (id 'y)) (id 'y))

(test (subst 'x (num 10) (parse '{with {x 1} x}))
      (parse '{with {x 1} x}))

(test (subst 'x (num 10) (parse '{with {y {+ x 1}} {+ x y}}))
      (parse '{with {y {+ 10 1}} {+ 10 y}}))

; run :: s-expr -> Num
(define (run prog [fundefs '()])
  (interp (parse prog) fundefs))

(test (run '{+ 4 2}) 6)
(test (run 3) 3)
(test (run '{+ {- 4 1} 2}) 5)
(test (run '{+ 1 {- 10 2/3}}) (+ 10 1/3))
(test/exn (run 'x) "free identifier")

(test (run '{with {x {+ 5 5}}
                  {+ x x}})
      20)
(test (run '{with {x {- 10 5}}
                  {+ x x}})
      10)
(test (run '{with {x {+ 5 5}}
                  {with {y {- x 3}}
                        {+ y y}}})
      14)
(test (run '{with {x 1}
                  {with {x x}
                        x}})
      1)

(test/exn (run '{with {x x} x}) "free")

; we want eager evaluation
(test/exn (run '{with {x y} 3}) "free")

; testing functions
(test/exn (run '{f 1}) "undefined function")

(test (interp (parse '{f 2}) (list (fundef 'f 'x (parse '{+ x x}))))
      4)

(test (run '{g {f 2}} (list (fundef 'f 'x (parse '{+ x x}))
                            (fundef 'g 'y (parse '{+ y 1}))))
      5)

; identifiers and function names live in different namespaces!
(test/exn (run '{f 10}
                (list (fundef 'f 'n (parse '{n n}))))
          "undefined function")


;b)

(define fib-fun
  (fundef 'fib'n
          (parse '{ifz n 1 {ifz {- n 1} 1 {+ {fib {- n 1}} {fib {- n 2}}}}})))

(define sum-fun
  (fundef 'sum 'n
          (parse '{ifz n n {+ n {sum {- n 1}}}})))

(test (run '{fib 5} (list fib-fun)) 8)
(test (run '{fib 12} (list fib-fun)) 233)
(test (run '{sum 42} (list sum-fun)) 903)

;c)

(define even-odd
  (list (fundef 'even 'n (parse '{ifz n 1 {odd {- n 1}}}))
        (fundef 'odd 'n (parse '{ifz n 0 {even {- n 1}}}))))

(test (run '{even 25} even-odd) 0)
(test (run '{odd 25} even-odd) 1)
(test (run '{even 42} even-odd) 1)

)
