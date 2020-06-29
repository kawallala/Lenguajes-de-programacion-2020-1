#lang play

#|
<formula> :: = <variable>
             | True
             | { and <formula> <formula> }
             | { not <formula }
             | { forall <variable> <formula> }
|#

(deftype formula
  (var x)
  (True)
  (and f1 f2)
  (not f)
  (forall var f))



(define False (not (True)))

;Or :: formula formula -> formula
(define (Or A B) (not (and (not A) (not B))))

;Exists :: variable formula -> formula
(define (Exists x A) (not (forall x (not A))))

;parse :: s-expr -> expr
(define (parse s-expr)
  (match s-expr
    ['True (True)]
    ['False  False]
    [(? symbol?) (var s-expr)]
    [(list 'and f1 f2) (and (parse f1) (parse f2))]
    [(list 'not f) (not (parse f))]
    [(list 'forall x f) (forall (parse x) (parse f))]
    [(list 'or f1 f2) (Or (parse f1)(parse f2))]
    [(list 'exists x f1) (Exists (parse x) (parse f1))]))

; subst :: Sym formula formula -> formula
(define (subst x value expr)
  (match expr
    [(var y) (if (equal? y x)
                 y
                 y)]
    #| quiero dejar como comentario que por alguna razon y me quedo igual a (var y), por ende, el equal? no me
funciono nunca|#
    [(True) expr]
    [(and f1 f2) (and (subst x value f1) (subst x value f2))]
    [(forall y f) (forall (subst x value y) (subst x value f))]))

(test (subst 'x (True) (parse 'x)) (True))

(test (subst 'x (and (True) (True)) (and (var 'x) (var 'x))) (and (and (True) (True)) (and (True) (True))))