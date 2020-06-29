#lang play
#|
1.a) No es recursiva por la cola puesto que, en el caso recursivo, queda pendiente una
     funcion a aplicarse sobre los resultados haciendo que queden llamados por realizar al
     momento de hacer el match no recursivo, particularmente, se realiza un llamado a cons

1.b) Recursión por la cola es cuando una funcion realiza
     un llamado a si misma que no necesita un nuevo frame en el stack de funciones.
     Llamado por la cola es cuando un grupo de metodos se llama entre ellos mediante llamados que
     van generando nuevos stacks por la cola, esto puede ser resuelto mediante un trampolin.
1.c)
|#
;concat :: list list -> list
;recibe dos listas y retorna su concatenacion
(define (concat l1 l2 [acum (list)])
  (match l1
    [(list) (match acum
              [(list) l2]
              [(cons h r) (concat l1 (cons h l2) r)])]
    [(cons h r) (concat r l2 (cons h acum))]))

(test (concat '(1 2 3) '(5 7 9)) '(1 2 3 5 7 9))
(test (concat '() '(1 2 3)) '(1 2 3))
(test (concat '(1 2 3) '()) '(1 2 3))

#|
1.d) La ventaja que presenta es que los llamados recursivos a cons estan en posicion de cola, por ende
     para el primer test el stack de ejecucion se ve como sigue
     (concat '(1 2 3) '(5 7 9) (list))
     (concat '(2 3) '(5 7 9) '(1))
     (concat '(3) '(5 7 9) '(2 1))
     (concat '() '(5 7 9) '(3 2 1))
     (concat '() '(3 5 7 9) '(2 1))
     (concat '() '(2 3 5 7 9) '(1))
     (concat '() '(1 2 3 5 7 9) '())
     '(1 2 3 5 7 9)
     De esta forma podemos ver que el stack se mantiene de tamaño constante, no aumentando en
     cada llamado como se hacia anteriormente.

1.e) La ventaja que obtenemos se pierde en Java puesto que este no realiza TRO, pidiendo nuevos
     frames para cada vez que realizamos un llamado a la funcion concat, y haciendo que, dadas listas
     los suficientemente grandes, se tenga un stack Overflow. Los beneficios de recursion por la cola
     podrian ser recuperados mediante la implementacion de un goto para evitar nuevos llamados a la
     funcion

1.f) TCO puede ser soportado mediante la implementacion de metodos "trampolin" que nos permiten saltar
     de una funcion a otra esperando un resultado, como el ejemplo visto en clases de las funciones
     seek y move
|#