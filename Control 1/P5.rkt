#lang play
;curry/2 [Any Any -> Any] -> [Any -> [Any -> Any]]
(define (curry/2 f)
  (lambda (x) (lambda (y) (f x y))))

;uncurry/2 [Any -> [Any -> Any]] -> [Any Any -> Any]
(define (uncurry/2 f)
  (lambda (x y) ((f x) y)))

;curry-wrong [Any ... Any -> Any] number -> [Any -> [Any ... [Any -> Any]...]]
(define (curry-wrong f arity [args '()])
  (if (zero? arity)
      (apply f args)
      (lambda (x) (curry-wrong f (sub1 arity) (if (equal? args '())
                                                  x
                                                  (list args x))))))

(define (resta a b) (- a b))
#| el resultado es -4, esto se debe a que los argumentos quedan en orden diferente al ingresar a la funcion generada|#
(((curry-wrong resta 2) 7) 3)