#lang play
;; macro de objetos y envios de mensajes en clases
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

;;; macro para el ejercicio, toma resolve, -> y reject como keywords, y captura result para su uso en
;;; el comportamiento de resolve o reject
;(defmac (promise prom (resolve -> etrue) (reject -> efalse))
;  #:keywords resolve -> reject
;  #:captures result
;  (OBJECT ([field expr #f]
;           [field calculated #f]) ;ocupamos void como valor de promesa no calculada 
;          ([method eval () (if calculated ;se calcula una vez el valor de la expresion
;                               (set! calculated #t)
;                               (begin
;                                 (set! expr prom)
;                                 (set! calculated #t)))] 
;           [method resolve(result) etrue]
;           [method reject (result) efalse]
;           [method unroll() (if calculated ;si el valor de la promesa no ha sido calculado se evalua
;                                (if expr
;                                    (-> self resolve expr)
;                                    (-> self reject expr))
;                                (begin
;                                  (-> self eval)
;                                  (if expr
;                                      (-> self resolve expr)
;                                      (-> self reject expr))))])))
;(define op (promise
;            (begin (printf "hi!")
;                   5)
;            (resolve -> (printf "the result is ~a!" result))
;            (reject -> (printf "failed :("))))
;(-> op unroll)
;(-> op unroll)

(defmac (promise prom (resolve -> etrue) (reject -> efalse))
        #:keywords resolve -> reject
        #:captures result
        (OBJECT ([field expr #f]
                 [field calculated #f]) ;ocupamos void como valor de promesa no calculada 
                ([method eval () (begin (printf "\n Eval\n")
                                        (if calculated ;se calcula una vez el valor de la expresion
                                     (begin (printf "Aquí debería hacer nada y estoy haciendo: Set")
                                            (set! calculated #t))
                                     (begin
                                       (set! expr prom)
                                       (set! calculated #t))))] 
                 [method resolve(result) etrue]
                 [method reject(result) efalse]
                 [method unroll()(begin (printf "\n Unroll \n")
                                       (if calculated ;si el valor de la promesa no ha sido calculado se evalua
                                            (if expr
                                                (-> self resolve expr)
                                                (-> self reject expr))
                                            (begin
                                              (-> self eval)
                                              (begin
                                                (printf "\n aquí debería llamar a eval si ya la llamé no debería hacer nada\nSi no lo he llamado debería mostrar ambos prints -> Solo esta Unroll")
                                                (if expr
                                                    (-> self resolve expr)
                                                    (-> self reject expr))))))])))

      (define op (promise

                  (begin (printf "hi!")

                         5)

                  (resolve -> (printf "the result is ~a!" result))

                  (reject -> (printf "failed :("))))

      (-> op unroll)

      (-> op eval)

      (-> op eval)

      (-> op eval)

      (-> op eval)

      (-> op unroll)

      (-> op unroll)

      

      (define f (promise

                 (begin #t

                         #f)

                  (resolve -> (printf "\nthe result is ~a!" result))

                  (reject -> (printf "\nfailed :("))))

      

      (-> f unroll)