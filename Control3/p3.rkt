#lang play
;; macro de objetos y envio de mensajes de clases
(defmac (OBJECT ([field fname fval] ...)
                ([method mname args body ...] ...))
  #:keywords field method
  #:captures self
  (letrec ([ self
             (let ([fname fval] ...)
               (let ([methods (list (cons 'mname (λ args body ...))...)])
                 (λ (msg . vals)
                   (let ([found (assoc msg methods)])
                     (if found
                         (apply (cdr found) vals)
                         (error "message not understood:" msg))))))])
    self))

(defmac (-> o m arg ...)
  (o 'm arg ...))


; a)
; se agrega con la finalidad de poder enviar mensajes a si mismo dentro del cuerpo de otro mensaje
; ya que, sin este, se requeriria ocupar el nombre del objeto que estamos creando, no permitiendo
; que los objetos puedan ser anonimos, en el ejemplo siguiente se ilustra esto
(define example-factory (OBJECT
                         ([field algo 1])
                         ([method make () (OBJECT ([field otro 2])
                                                  ([method me () self]))])))
; la fabrica crea objetos anonimos que tienen un mensaje para devolver una referencia a si mismos,
; sin embargo, si no existiera self, no tendriamos un identificador que permita simular el mismo comportamiento
; b)
; no tendria sentido que los objetos tuvieran self, puesto que siempore podrian ser referenciados
; con el nombre que los define


; c)
; El polimorfismo que los objetos manifiestan es que, se puede enviar un mensaje reconocible a un
; objeto, sin tener certeza de que otros mensajes ocupa, por ende, mientras dos tipos de objeto
; pueden entender mensajes distintos, en el contexto de un unico mensaje, se comportan como el mismo 

(define duck (OBJECT ([field color "white"])
              ([method quack() (printf "quakc!")])))

(define gooose (OBJECT ([field color "white and black"])
              ([method quack() (printf "quakc!")]
               [method attack() (printf "peace was never an option")])))

(-> duck quack)
(-> gooose quack)
; en el caso anterior, tenemos que duck y swam tienen definiciones diferentes, pero ante el mensaje
; quack, se comportan como si fueran el mismo objeto


;d)
; La parametrizacion es importante para que el self pueda ser llamado en el interior del metodo correspondiente,
; en java, la parametrizacion se hace de forma oculta al programador, pero tambien existe self como parametro en
; los metodos de una clase


;e)
; En un sistema que solo tiene objetos y usa funciones como fabrica esta el problema que cada objeto
; creado por la misma fabrica tiene los mismos lambdas encapsulados en su definicion, en cambio, en
; un sistema OOP con clases podemos tener los lambdas compartidos en la definicion de la clase, y el
; objeto solo es capaz de delegar los mensajes a su clase padre.