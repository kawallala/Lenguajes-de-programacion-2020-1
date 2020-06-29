#lang play
(define (pair-add1 p)
  (cons (+ (car p) 1) (+ (cdr p) 1)))
(equal? (pair-add1 (cons 0 1)) (cons 1 2))

(define (length-string l)
  map string-length l)

(define (sum l)
  (foldl + 0 l))
(equal? (sum '()) 0)

(define (string-concat l)
  (foldr string-append "" l))

(equal? (string-concat (list "hola" "mundo")) "holamundo")

(define (>0 l)
  (filter (lambda (x) (and (real? x) (< 0 x))) l))

(define (map-list f l)
  (if (empty? l)
      empty
      (cons (f (first l)) map-list(f (rest l)))))

(define (foldl-list f init l)
  (if (empty? l)
      init
      (foldl-list f (f init (first l)) (rest l))
      ))
(define (foldr-list f init l)
  (if (empty? l)
      init
      (f (first l) (foldr-list f init (rest l)))))

(define (node? t)
  (cons? t))
