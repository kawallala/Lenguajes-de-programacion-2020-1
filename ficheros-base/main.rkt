 #lang play
(require "machine.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <s-expr>} [: <type>] <expr>}
         | {<expr> <expr>}

<type> ::= Num
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s)
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))

#|
1. Funciones de primera clase con tipos declarados
|#

;parse-type:: s-expr -> type o error
;parsea la gramatica de tipos, en caso de error
;retorna "Parse error""
(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list a '-> b) (TFun (parse-type a) (parse-type b))]
    [_ (error "Parse error")]))

;parse:: s-expr -> Expr
;parsea la gramatica completa del lenguaje
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ expr1 expr2) (add (parse expr1) (parse expr2))]
    [(list '- expr1 expr2) (sub (parse expr1) (parse expr2))]
    [(list 'with (list i ': t e) b) (app (fun i (parse-type t) (parse b) #f) (parse e))]
    [(list 'fun (list i ': ti) ': tb b) (fun i (parse-type ti) (parse b) (parse-type tb))]
    [(list 'fun (list i ': ti) b) (fun i (parse-type ti) (parse b) #f)]
    [(list a b) (app (parse a) (parse b))]))

;parse:: Type -> type
;recibe un tipo y lo regresa en la notacion correcta
(define (prettify type)
  (match type
    [(TNum) 'Num]
    [(TFun a b) (list (prettify a) '-> (prettify b))]))

#|
2. Verificación de Tipos
|#
(deftype Tenv
  (mtTenv)
  (aTenv id type next))

(define empty-Tenv mtTenv)
(define extend-env aTenv)

;lookup-env:: Symbol Tenv -> Type
(define (lookup-env id env)
  (match env
    [(mtTenv) (error "Type error: free identifier:" id)]
    [(aTenv x t n) (if (equal? x id)
                       t
                       (lookup-env id n))]))
;throw-error:: String String String String -> error
; funcion de ayuda para simplificar los errores
(define (throw-error id pos s1 s2)
  (error (format "Type error in expression ~a position ~a: expected ~a found ~a" id pos s1 s2)))

(define (typeof expr [env (empty-Tenv)])
  (match expr
    [(num x) (TNum)]
    [(id x) (lookup-env x env)]
    [(add x y) (if (equal? (TNum) (typeof x env))
                   (if (equal? (TNum) (typeof y env))
                       (TNum)
                       (throw-error "+" "2" "Num" (prettify (typeof y env))))
                   (throw-error "+" "1" "Num" (prettify (typeof x env))))]
    [(sub x y) (if (equal? (TNum) (typeof x env))
                   (if (equal? (TNum) (typeof y env))
                       (TNum)
                       (throw-error "-" "2" "Num" (prettify (typeof y env))))
                   (throw-error "-" "1" "Num" (prettify (typeof y env))))]
    [(fun i ti b tb) (def ttb (typeof b (extend-env i ti env)))
                     (if tb
                          (if (equal? ttb tb)
                              (TFun ti ttb)
                              (throw-error "fun" "1" (prettify tb) (prettify ttb)))
                          (TFun ti ttb))]
    [(app f e) (match (typeof f)
                 [(TFun arf ret) (def tret (typeof e env))
                                 (if (equal? tret ret)
                                     ret
                                     (throw-error "app" "2" (prettify ret) (prettify tret)))]
                 [_ (throw-error "app" "1" "(?T -> ?S)" (prettify (typeof f)))])]))

;typecheck :: Type -> Type
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


#|
3. Compilación
|#

;position:: List[Any] Any -> Any
(define (position l x [n 0])
  (match l
    ['() (error "Free identifier:" x)]
    [(list-rest y l) (if (equal? x y)
                         (acc n)
                         (position l x (add1 n)))]))
;deBruijn: expr -> expr
(define (deBruijn expr [bound '()])
  (match expr
    [(num x) expr]
    [(id x) (position bound x)]
    [(add x y) (add (deBruijn x bound) (deBruijn y bound))]
    [(sub x y) (add (deBruijn x bound) (deBruijn y bound))]
    [(fun i ti b tb) (fun-db (deBruijn b (cons i bound)))]
    [(app f e) (app (deBruijn f bound) (deBruijn e bound))]))

(define (compile expr)
  (match expr
    [(num n) (INT-CONST n)]
    [(add x y) (flatten (list (compile y) (compile x) (ADD)))]
    [(sub x y) (flatten (list (compile y) (compile x) (SUB)))]
    [(acc x) (ACCESS x)]
    [(app f e) (flatten (list (compile e) (compile f) (APPLY)))]
    [(fun-db b) (CLOSURE (flatten (list (compile b) (RETURN))))]))

(define (typed-compile s-expr)
  (begin
    (typecheck s-expr)
    (compile (deBruijn (parse s-expr)))))
