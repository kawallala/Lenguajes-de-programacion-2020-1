#lang play

(print-only-errors #t)

;1. Arboles Binarios.
;a)

(deftype BinTree
  (leaf val)
  (node left val right))

(define bt0
  (node (leaf 3) 7 (node (leaf 5) 12 (leaf 2))))

(define bt1
  (leaf "Hello world"))

(define bt2
  (node (node (leaf '(1 2 3)) 'a (leaf '()))
        'x
        (node (node (leaf '(2)) 'z (leaf '( 42 7 8 20 )))
              'c
              (leaf '(17 23)))))

;b)
#|
 map-bt : (A -> B) x BinTree[A] -> BinTree[B]
|#
(define (map-bt f bt)
  (match bt
    [(leaf val) (leaf (f val))]
    [(node l val r) (node (map-bt f l) (f val) (map-bt f r))]))

(test (map-bt number->string bt0)
      (node (leaf "3") "7" (node (leaf "5") "12" (leaf "2"))))
(test (map-bt (lambda (x) (< 4 x)) bt0)
      (node (leaf #f) #t (node (leaf #t) #t (leaf #f))))
(test (map-bt string-length bt1)
      (leaf 11))

; Como definir un fold sobre una estructura recursiva resultando en un típo B ?
; Un fold "agrega" todos los datos dentro de una estructura
; para hacer eso se necesita una funcíon explicando como
; agregar los argumentos de cada constructores

; Ejemplo: list A = nil | cons A (list A)
; - nil no toma argumento así que solamente necesitas un valor (inicial) de típo B
; - cons toma dos argumentos, un A y un B (el resulto de agregar la lista) y devuelve otro B
;
; resulta en fold-list : B x (A x B -> B) x List[A] -> B

; Para BinTree tenemos 2 constructores
; - leaf toma un argumento de un típo A_leaf, así que coresponde
;   a una funcíon A_leaf -> B
; - node toma 3 argumentos (2 subarboles y un valor de típo A-node)
;   coresponde a una funcíon B x A_node x B -> B


#|
  fold-bt : (Leaf -> A) x (A x Node x A -> A) x BinTree[Leaf, Node] -> A
|#
(define (fold-bt fleaf fnode bt)
  (match bt
    [(leaf val) (fleaf val)]
    [(node l val r) (fnode (fold-bt fleaf fnode l) val (fold-bt fleaf fnode r))]))

(test (fold-bt list
               (lambda (l v r) (append l (list v) r))
               bt0)
      '(3 7 5 12 2))
(test (fold-bt length
               (lambda (l v r) (+ l (string-length (symbol->string v)) r))
               bt2)
      14)
;c)
#|
BinTree-full? : Any -> Boolean
|#

(define (BinTree-full? bt)
  (and (BinTree? bt)
       (match bt
         [(? leaf?) #t]
         [(node l _ r) (and (BinTree-full? l) (BinTree-full? r))])))

; Otras soluciones

;; (define (BinTree-full? bt)
;;   (match bt
;;     [(leaf val) #t]
;;     [(node l val r) (and (BinTree-full? l)
;;                          (BinTree-full? r))]
;;     [_ #f]))

;; (define (BinTree-full? bt)
;;   (or (leaf? bt)
;;       (and (node? bt)
;;            (BinTree-full? (node-left bt))
;;            (BinTree-full? (node-right bt)))))

(test (map BinTree-full? (list bt0 bt1 bt2)) '(#t #t #t))
(test (BinTree-full? (node (leaf 2) 'a 5)) #f)

;d)
#|
BinTree-full-pred? : (Any -> Boolean) x (Any -> Boolean) x Any -> Boolean
|#
(define (BinTree-full-pred? pleaf pnode bt)
  (and (BinTree? bt)
       (match bt
         [(leaf v) (pleaf v)]
         [(node l v r) (and (BinTree-full-pred? pleaf pnode l)
                            (pnode v)
                            (BinTree-full-pred? pleaf pnode r))])))

(test (BinTree-full-pred? (lambda (x) #t) (lambda (x) #t) bt0) #t)
(test (BinTree-full-pred? list? symbol? bt2) #t)
(test (BinTree-full-pred? number? string? bt0) #f)


; 2. Manipulacíon de AST.
(module Expr play ;; Definimos los simbolos localmente
(print-only-errors #t)
;a)

(deftype Expr
  (num val)
  (binop op left right))

; op debería ser parte de la lista de operaciones
(define operations '(+ - / *))

;; Ejemplo:
;; '{+ 3 4} -> (binop '+ (num 3) (num 4))

;b)
#|
parse : sexpr -> Expr
|#
(define (parse sexpr)
  (define (arith-op? op) (member op operations))
  (match sexpr
    [(? number? val) (num val)]
    [(list (? arith-op? op) l r) (binop op (parse l) (parse r))]))


(define e0 (parse '{+ {* 8 9} {- 4 7}}))
(define e1 (parse '{/ 42 {- 3 3}}))

(test e0 (binop '+ (binop '* (num 8) (num 9)) (binop '- (num 4) (num 7))))
(test e1 (binop '/ (num 42) (binop '- (num 3) (num 3))))
(test (parse 3) (num 3))

;c) Los dos tipos BinTree y Expr tienen la misma estructura, son solamente los
;nombres de los constructores y la posicíon de los argumentos que cambian. En
;particular podríamos implementar fold-Expr y map-Expr con un codigo muy similar
;a lo de BinTree.

;d)
#|
opt : Expr -> Expr
Optimiza todas las occurencias de {+ 0 x}, {+ x 0}, {* 1 x}, {* x 1} con x
|#
(define (opt e)
  (match e
    [(? num?) e]
    [(binop '+ (num 0) e) e]
    [(binop '+ e (num 0)) e]
    [(binop '* (num 1) e) e]
    [(binop '* e (num 1)) e]
    [(binop op l r)
     (def lopt (opt l))
     (def ropt (opt r))
     (if (and (equal? l lopt) (equal? r ropt))
         e
         (opt (binop op lopt ropt)))]))

(test (opt (parse '{+ {- 7 2} {+ 0 0}})) (parse '{- 7 2}))
(test (opt (parse '{/ {* {+ 1 0} {- 3 2}} 7})) (parse '{/ {- 3 2} 7}))

) ;; Fin del module Expr


; 3. Booleanos
;a)

(deftype Expr
  (num val)
  (bool val)
  (binop op left right))

#|
parse : sexpr -> Expr
|#
(define operators '(+ - / * and or
                      ; Modificacíon para pregunta c)
                      < = >))

(define (parse sexpr)
  (define (operator? op) (member op operators))
  (match sexpr
    [(? number? val) (num val)]
    [(? boolean? val) (bool val)]
    [(list (? operator? op) l r) (binop op (parse l) (parse r))]))

(define e0 (parse '{+ {* 8 9} {- 4 7}}))
(define e1 (parse '{/ 42 {- 3 3}}))
(define e2 (parse '{or #t {and #f #f}}))

(test e0 (binop '+ (binop '* (num 8) (num 9)) (binop '- (num 4) (num 7))))
(test e1 (binop '/ (num 42) (binop '- (num 3) (num 3))))
(test (parse 3) (num 3))
(test e2 (binop 'or (bool #t) (binop 'and (bool #f) (bool #f))))

#|
eval : Expr -> or/c boolean? number?
|#

(define binop-map
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons 'or (lambda (x y) (or x y)))
   (cons 'and (lambda (x y) (and x y)))
   ; Modificacíon para pregunta c)
   (cons '= equal?)
   (cons '< <)
   (cons '< >)
   ))

(define (eval e)
  (define (eval-binop op) (cdr (assoc op binop-map)))
  (match e
    [(num val) val]
    [(bool val) val]
    [(binop op l r) ((eval-binop op) (eval l) (eval r))]))

(test (eval e0) 69)
(test (eval e2) #t)


;c) Modificamos operators y binop-map

#|
run : s-expr -> or/c boolean? number?
|#
(define (run sexpr)
  (eval (parse sexpr)))


(test (run {and {< {+ 2 3} 7} #f}) #f)


;d)
(define expr1 '42) ; tiene tipo 'num
(define expr2 '{or #t {and #f #f}}) ; tiene tipo 'bool
(define expr3 '{+ 72 {- {* 8 3} 2}}) ; tiene tipo 'num
(define expr4 '{/ 7 0}) ; tiene tipo 'num
(define expr5 '{+ #t 6}) ; no tiene típo (no se puede añadir un boolean)
(define expr6 '{and {< {+ 2 3} 7} #f}) ; tiene tipo 'bool
(define expr7 '{= #t #f}) ; no tiene típo, = solamente compara numeros en est ejercicio (pero con la implementacíon de eval se puede evaluar)
(define expr8 '{or {> 3 {- 5 4}} {+ 1 2}}) ; no tiene típo (pero con la implementacíon de eval se puede evaluar a un boolean)

(define (type? s) (or (equal? s 'num) (equal? s 'bool)))

;e)
#|
Expr->string : Expr -> string
|#
(define (Expr->string e)
  (match e
    [(num val) (number->string val)]
    [(bool val) (if val "#t" "#f")]
    [(binop op l r) (string-append "{" (symbol->string op)
                                   (Expr->string l)
                                   (Expr->string r) "}" )]))


(define binop-type
  (list
    (cons '+ (list 'num 'num 'num))
    (cons '- (list 'num 'num 'num))
    (cons '* (list 'num 'num 'num))
    (cons '/ (list 'num 'num 'num))
    (cons 'or (list 'bool 'bool 'bool))
    (cons 'and (list 'bool 'bool 'bool))
    (cons '= (list 'num 'num 'bool))
    (cons '< (list 'num 'num 'bool))
    (cons '> (list 'num 'num 'bool))))

#|
infer-type : Expr -> type?
|#
(define (infer-type e)
  (define (infer-binop-type op) (cdr (assoc op binop-type)))
  (define (type-error e ty)
    (error (string-append (Expr->string e)
                          " doesn't have type "
                          (symbol->string ty))))
  (match e
    [(num _) 'num]
    [(bool _) 'bool]
    [(binop op l r)
     (def (list ltype rtype outtype) (infer-binop-type op))
     (and (or (equal? ltype (infer-type l)) (type-error l ltype))
          (or (equal? rtype (infer-type r)) (type-error r rtype))
          outtype)]))

(test (infer-type (num 42)) 'num)
(test (infer-type (binop '+ (num 42) (num 3))) 'num)
(test (infer-type (binop 'and (bool #t) (binop '< (num 3) (num 7)))) 'bool)


;4) Identificadores enlazados, identificadores libres.


(define t '{with {x 3}
                 {with {y 7}
                       {with {z 2}
                             {- {+ x y} z}}}})
(define u '{with {x y}
                 {with {y x}
                       {+ y x}}})

(define v '{+ {with {x 3} x}
              {- {with {y 2} x}
                 {with {x 7} x}}})

;a) El primer y es libre en u, el tercer x es libre en v.

;b) El alcance de y en t es {with {z 2} {- {+ x y} z}}.
; El alcance de y en u es {+ y x}. No tiene sentido hablar del alcance del primer y porque es libre.
; El alcance de y en t es x.

;c) En t, el segundo x es enlazado por el primero
; En u, el segundo y tercero x son enlazados por el primero
; En v, el segundo x es enlazado por el primero x y el quinto x es enlazado por el cuarto x
