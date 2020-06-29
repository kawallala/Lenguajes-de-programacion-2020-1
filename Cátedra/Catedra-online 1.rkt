#lang play
(print-only-errors #f)
#|
<btree> ::= (leaf <val>) |
            (node <val> <btree> <btree>)
|#

(deftype BinTree
  (leaf value)
  (node value left right))
; NO PATTERN MATCHING
#|(define (f t)
  (if (leaf? t)
      ...
      ... (f (node-left t))... (f (node-right t)) ...))
|#
; PATTERN MATCHING
#|(define (f t)
  (match t
    [(leaf v) ...]
    [(node v l r)... (f l)... (f r)...]))
|#

; contains? :: BinTree x Val -> Bool
(define (contains? t val)
  (match t
    [(leaf v) (equal? v val)]
    [(node v l r)(or (equal? v val)
                     (contains? l val)
                     (contains? r val))]))
;------------------------------------------
#|
  <expr ::= (num <n>)
          | (add <expr> <expr>)
          | (sub <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r))


; calc : Expr -> num
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(div l r) (if (equal? r (num 0))
                   (error "no se permite dividir por 0")
                   (/ (calc l) (calc r)))]))

(test/exn (calc (div (num 10) (num 0)))
      "no se permite dividir por 0")

(test (calc (div (num 10) (num 2)))
      5)

(test (calc (add (num 10) (num 20)))
      30)

(test (calc (add (num 10) (sub (num 20) (num 10))))
      20)
; (add (num 10) (num 20)) = 30