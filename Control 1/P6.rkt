#lang play
;sumatoria :: number [number -> number] number -> number
(define (sumatoria a f b)
  (if (> a b)
      0
      (+ (f a) (sumatoria (+ a 1) f b))))

;numero-perfecto? number -> boolean
(define (numero-perfecto? n [k 1] [t 0])
  (if (equal? k n)
      (equal? n t)
      (if (divides? k n)
          (numero-perfecto? n (+ k 1) (+ t k))
          (numero-perfecto? n (+ k 1) t))))
