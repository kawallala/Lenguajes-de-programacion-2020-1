#lang racket

;1)
;a)
#|
-(cons 'a 'b) representa un par, con el elemento de la izquierda es el simbolo a y el elemento de la derecha es el simbolo b
-(list 'a 'b) representa una lista, donde el primer elemento es el simbolo a y el segundo elemento es el simbolo b
-Recordar que una lista se define como una concatenación de pares, donde el ultimo elemento es el elemento vacio,
por lo tanto su representación de pares seria: (cons 'a (cons 'b '()))

- la notacíon '( elementos ) es el resulto de llamar la funcíon list
'(a b c d f) = (list a b c d f)
|#

;b)
#|
'((a b) c)
'((a . (b . ())) . (c . ()))
(list (list a b) c)

es un par donde el elemento de la izquierda es (a . (b . ())) y el de la derecha es (c . ())
El de la izquierda es la lista (a . (b . ()))
por lo tanto queda la lista ((a b) c)
|#

;c)
(define l (list '(a b c) '(d e f) '(g h i)))

(car (cdr (cdr (car l)))) ; = c

(car(cdr (car (cdr l)))) ; = e

;d)
;'(c)
(cons 'c '())

;'(a b)
(cons 'a (cons 'b '()))

;'((a b) (c))
(cons (cons 'a (cons 'b '())) (cons 'c '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2)
#|
pair-add1: Pair[Int][Int] -> Pair[Int][Int]
Recibe un par de números y retorna un nuevo par dónde los dos elementos fueron incrementados en
|#
(define (pair-add1 p)
  (cons (+ (car p) 1) (+ (cdr p) 1)))

(equal? (pair-add1 (cons 0 1)) (cons 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3)
;a)
#|
add1-list : List[Nat] -> List[Nat]
|#
(define (add1-list l)
  (map add1 l))

(equal? (add1-list '()) '())
(equal? (add1-list (list 1 2 3)) (list 2 3 4))

;b)
#|
length-strings : List[String] -> List[Int]
|#
(define (length-strings l)
  (map string-length l))

(equal? (length-strings '("hola" "mundo")) '(4 5))

;c)
#|
sum : List[Nat] -> Nat
|#
(define (sum l)
  (foldl + 0 l))

(equal? (sum '()) 0)
(equal? (sum '(1 2 3 4 5)) 15)

;d)
#|
string-concat : List[String]-> String
|#
(define (string-concat l)
  (foldr  string-append "" l))

(equal? (string-concat (list "hola" "mundo")) "holamundo")

;e)
#|
>0 : list[Number] -> list[Number]
filtra la lista de number para guardar solamente los positivos
|#
(define (>0 l)
  (filter (lambda (x) (and (real? x) (< 0 x))) l))

(equal? (>0 '(-2 3 7 1+i 42)) '(3 7 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;4)
#|
map-list : (A -> B) -> List[A] -> List[B]
|#
(define (map-list f l)
  (if (empty? l)
      empty
      (cons (f (first l))  (map-list f (rest l)))
      ))

; l = (cons (first l) (rest l))

(equal? (map-list add1 '(1 2 3)) (map add1 '(1 2 3)))
(equal? (map-list number->string '()) '())
(equal? (map-list not '(#t #f #t #t #t #f)) '(#f #t #f #f #f #t))

#|
foldl-list : (A ->B -> B) -> B -> List[A] -> B

foldl-list f b0 (cons a0 as) = foldl-list f (f a0 b0) as
|#
(define (foldl-list f init l)
  (if (empty? l)
      init
      (foldl-list f (f (first l) init) (rest l))))

(equal? (foldl-list + 0 '()) 0)
(equal? (foldl-list (lambda (x y) (or x y)) #t '()) #t)
(equal? (foldl-list + 0 '(1 2 3 4 5 6)) (sum '(1 2 3 4 5 6)))
(equal? (foldl-list cons '() '(1 2 3 4)) '(4 3 2 1))

#|
foldr-list : (A -> B -> B) -> B -> List[A] -> B

foldr-list f b0 (append as '(a0)) = foldr-list f (f a0 b0) as
|#
(define (foldr-list f init l)
  (if (empty? l)
      init
      (f (first l) (foldr-list f init (rest l)))))

(equal? (foldr-list + 0 '()) 0)
(equal? (foldr-list (lambda (x y) (or x y)) #t '()) #t)
(equal? (foldr-list + 0 '(1 2 3 4 5 6)) (sum '(1 2 3 4 5 6)))
(equal? (foldr-list cons '() '(1 2 3 4)) '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;5)
;a)
#|
leaf : Any -> BinTree[Any]
|#
(define (leaf val)
  val)

#|
node : BinTree[A] -> BinTree[A] -> BinTree[A]
|#
(define (node left right)
  (cons left right))

#|
node? : BinTree[A] -> Boolean
|#
(define (node? t)
  (cons? t))

(equal? (node? (node 3 5)) #t)

(define arbol (node (node (leaf 4) (leaf 3)) (leaf 5)))

;b)
#|
Patrón general para definir una funcíon recursiva sobre un árbol binario

f : BinTree[A] -> ...
(define (f bt)
  (if (node? bt)
      ;; caso recursivo bt = node (car bt) (cdr bt)
      ... (f (car bt))... (f (cdr bt)) ...

      ;; caso de una hoja bt = val
      ...
      ))
|#

;c)
#|
bt-contains? : A -> BinTree[A] -> Boolean
|#
(define (bt-contains? val bt)
  (if (node? bt)
      (or (bt-contains? val (car bt)) (bt-contains? val (cdr bt)))
      (equal? val bt)))

(define t (node (node (leaf 3) (node (leaf 1) (leaf 5))) (node (leaf 2) (leaf 7))))

(equal? (bt-contains? 2 t) #t)
(equal? (bt-contains? 42 t) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;6)
;a)
#|
map-bt : (A -> B) -> BinTree[A] -> BinTree[B]
Applica la función que recibe a todxs las hojas del árbol.
|#
(define (map-bt f bt)
  (if (node? bt)
      (node (map-bt f (car bt)) (map-bt f (cdr bt)))
      (f bt)))

(equal? (map-bt add1 t) (node (node 4 (node 2 6)) (node 3 8)))

#|
fold-bt : (A -> A -> A) -> BinTree[A] -> A
|#
(define (fold-bt f bt)
  (if (node? bt)
      (f (fold-bt f (car bt)) (fold-bt f (cdr bt)))
      bt))

(equal? (fold-bt + t) 18)

#|
fold-bt-full : (A -> B) -> (B -> B -> B) -> BinTree[A] -> B
|#
(define (fold-bt-full g f bt)
  (if (node? bt)
      (f (fold-bt-full g  f (car bt)) (fold-bt-full g f (cdr bt)))
      (g bt)))

(equal? (fold-bt-full number->string string-append t) "31527")

;b)
#|
exist : (A -> Boolean) -> BinTree[A] -> Boolean
|#
(define (exist-bt p bt)
  (fold-bt-full p (lambda (x y) (or x y)) bt))

(equal? (exist-bt (lambda (x) (< 6 x)) t) #t)
(equal? (exist-bt (lambda (x) (> -2 x)) t) #f)

#|
forall : (A -> Boolean) -> BinTree[A] -> Boolean
|#
(define (forall-bt p bt)
  (fold-bt-full p (lambda (x y) (and x y)) bt))

(equal? (forall-bt (lambda (x) (< -2 x)) t) #t)
