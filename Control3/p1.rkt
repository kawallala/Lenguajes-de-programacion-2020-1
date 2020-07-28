#lang play
;; a)
(defmac (switch body [case pred -> expr]... [default -> ef])
  #:keywords case ->
  #:captures val ;este corresponde al cambio necesario para la parte b), sin esta linea es la parte a)
  (let ([val body])
    (match body
      [(? pred) expr]...
      [_ ef])))

(switch (- 10 17)
        [case positive? -> 7]
        [case negative? -> 40]
        [case number? -> 1729]
        [default -> 8])

; c)
;Que el sistema de macros sea higienico implica que los identificadores
;que las macros ocupan no interfieren con aquellos en el scope del programa,
;al momento de utilizar la macro se consideran diferentes, un ejemplo de esto es
;lo siguiente
(define a (box 1))

(defmac (change a)
  (set-box! a 2))

(define b (box 3))
(change b)
(unbox b) ; 2
(unbox a) ; 1
; En el ejemplo anterior la macro que creamos utiliza el identificador a, pero no afecta el
; valor presente en la caja de a, si las macros no fueran higienicas (o capturamos a proposito el identificador a)
; encontrariamos que el valor de a fue modificado mientras que el valor de b se mantuvo

; d)
;No pueden serlo porque se ocupan optimizaciones para no evaluar la totalidad de las expresiones que se utilizan,
;particularmente en and, se evalua cada expresion hasta encontrar una falsa. Esto implica que no puede ser usado como funcion
;de primera clase en el lenguaje Racket