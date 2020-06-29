#lang play

(define x 10)

(define (double x)
    (+ x x))

(define (addn n)
    (\lambda (m)
      (+ n m)))