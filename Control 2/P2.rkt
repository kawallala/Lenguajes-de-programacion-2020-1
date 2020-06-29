#lang play
#|
2.a) El problema de este interprete es que, al momento de interpretar la expresión donde se aplica la
     función (e) se esta usando el store con el que se llamó al interprete, y no el store generado por
     la interpretación de la función a usarse (f-sto) esto producirá que cambios realizados sobre el store
     mediante la función no sean detectados por la interpretación de la expresión.
     Un programa que ilustra este problema es el siguiente:
|#
'{with {redn {fun {n} {fun {m} {- n m}}}}
       {{redn 10} 2}}
             
#|
El resultado de este programa es 0, puesto que el valor del store donde se guarda el valor 10 no es visto
cuando se coloca el valor 2, haciendo que el identificador m tenga la misma dirección que n, y ambos apunten
al mismo valor, 2.
La solución propuesta es que en la interpretación de la expresión "e" se utilice el f-sto
en vez del store que venía desde el llamado al interp, y además la interpretacion del cuerpo de
la clausura sea extendiendo el store resultante ("e-sto") en vez del de la función.

2.b) Un programa para ver el uso de call-by-value o call-by-reference será uno donde se intente,
     dentro de una función, modificar un valor que sera ocupado fuera de esta:
|#
'{with {y 0}
       {with {f {fun {x}
                     {set x 10}}}
             {seqn {f y}
                   {+ y y}}}}
#|
En este programa, si estamos ocupando call-by-value el resultado sera 0, puesto que en la funcion
f se crea una copia de la direccion de "y", entonces las modificaciones que se realizen a la
variable seran solamente dentro del scope de la funcion, en cambio, si realizamos call-by-reference
tendriamos como resultado 20, puesto que se entrego la referencia del identificador y, haciendo que las
modificaciones que se realicen se mantengan despues de haber realizado el cuerpo de la funcion

2.c) El interprete ocupa una semantica call-by-value, esto se puede deducir debido al uso
de malloc para crear un nuevo registro en la memoria donde se guarda el valor al que hace referencia
el nuevo identificador que agregamos al env, entonces las modificaciones a este valor seran ignoradas fuera de la funcion

2.d) Lo que se tendria que realizar es una busqueda de la direccion a la cual el identificador esta apuntando
en el ambiente (lookup-env) y hacer la interpretacion del cuerpo de la clausura en el mismo store recibido
desde la interpretacion de la funcion, con un ambiente extendido con un identificador apuntando a la misma
direccion que el identificador original.

2.e) Esto implica que el paso de parametros en C es call-by-reference por defecto, haciendo que todo parametro
que sea entregaod a una funcion pueda ser modificado para su uso fuera de esta

2.f) Esto no era posible puesto que requeriria un cambio a un scope dinamico, lo cual nos produce efectos no
deseados en nuestra implementacion
|#

            