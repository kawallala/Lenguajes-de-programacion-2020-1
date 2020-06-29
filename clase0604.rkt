#lang play

; macros: extensiones sintacticas

; macros en C
; mc :: String -> String "search/replace de vim/notepad"
; -> no hay ninguna garantía que la string de salida sea
; un programa "valido" (sintacticamente)
; -> es sensible a indentacion, nuevas lineas, comments, etc.

; macros en Scheme (antes: Lisp, pero Scheme tiene importantes mejoras)
; ms :: AST -> AST
; -> al menos el output parsea (s-expr)
; -> no depende de la sintaxis concreta

; Ejemplo: let con un sólo binding
(defmac
  (slet [id val] body) ; patron
                       ; metavariables: id val body
  ;(let ([id val]) body)) ; equivalente
  ((λ (id) body) val)) ; template

(slet [x 10]
  (+ x x))
;; --> ((λ (x) (+ x x)) 10)

; timer: no puede ser una funcion
; (las funciones toman sus argumentos ya evaluados!!)
(defmac (my-time e) ; metavariable: e (patron trivial)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

(my-time (expt 2 100000000))

;; 1. Ellipsis

; hagan click en macro stepper y observen la expansión de slet y my-time
; (asegurense que el "macro hiding" esta en "standard"

; (mlet ([x 10][y 20][z 30]) (+ x y z))
; --> ((λ (x y z) (+ x y z)) 10 20 30)
; uso de *ellipsis* para expresar "0 o mas" de la s-expr que esta antes
(defmac (mlet ([id val] ...) body)
  ((λ (id ...) body) val ...))

(mlet ([x 10][y 20][z 30]) (+ x y z))
(mlet () 1)

;; 2. Ellipsis con restricción de al menos 1

; ahora una version de mlet que exige al menos 1 binding
(defmac (nlet ([id0 val0][id val] ...) body)
  ((λ (id0 id ...) body) val0 val ...))

(nlet ([x 10][y 20][z 30]) (+ x y z))
; (nlet () 1) <-- error de sintaxis!


;; 3. Keywords

(defmac (cond2 (c e1) (else e2))
  #:keywords else
  (if c e1 e2))

; (cond2 (c e1) (else e2))
(cond2 ((> 3 5) (+ 20 10))
       (else 10))

; este ejemplo es error de sintaxis si `else` es un keyword
;(cond2 ((> 3 5) (+ 20 10))
;       ((/ 2 0) 10))   ; (if (> 3 5) (+ 20 10) 10)


;; 4. PITFALL 1: cuidado con duplicar las expresiones!
(defmac (my-or e1 e2)
  (let ([res e1])  ; sin el let, se evalua e1 dos veces
    (if res res e2)))

(my-or (begin (print "hola")
              2)
       #f)

;; 5. HYGIENIC MACROS (o, como preservar scope lexico en un mundo expansible)

; ¿que pasa si el cliente justo usa el mismo nombre de variable usado por la macro??
(let ([res 10])
  (my-or false res))

; a que expande esto?
; a esto?? no
(let ([res 10])
  (let ([res false])  
    (if res res res)))
  ; esto no es lo que queremos, y sería el resultado en un sistema no-higienico (C, Lisp)

; respuesta: expande a esto!
(let ([res 10])
  (let ([res_1 false])   ; en el macro stepper se usan colores en vez de _1, _2, etc.
    (if res_1 res_1 res)))







