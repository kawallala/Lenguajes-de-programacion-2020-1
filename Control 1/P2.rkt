#lang play
#|
a)
El programa tiene evaluacion temprana (o eager), un programa que tendria una respuesta distnta seria el siguiente
|#
(with y x (+ 1 2))
#| con la evaluacion implmentada este programa se caeria |#

#|
b)
Habria que tener una forma (pueden ser enviroments) de guardar la expresion que corresponde a bid, asociada
a el, hasta el momento de su uso, donde se ocupa el valor de la expresion
|#