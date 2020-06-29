#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;Promesas
(deftype promise
  (exprv arg env cache))

; list-to-cons:: s-expr -> s-expr
; transforma el azucar sintactico de list a una conjuncion de Cons
(define (list-to-cons l)
  (match l
    ['() (list 'Empty)]
    [_ (append (list 'Cons) (list (car l)) (list (list-to-cons (cdr l))))]))
  
    
;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'list vals ...) (parse (list-to-cons vals))]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]))

; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

;l-to-pattern :: s-expr -> Pattern
; transform a list to a Pattern formed by Cons
(define (l-to-pattern l)
  (match l
    [(list a) (constrP 'Cons (list (parse-pattern a) (parse-pattern '{Empty})))]
    [(list vals ...) (constrP 'Cons (list (parse-pattern (first vals)) (l-to-pattern (cdr vals))))]))

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    [(list 'list vals ...)  (l-to-pattern vals)]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

;fix :: list list -> list
; Usa la lista de ids para convertir en promesas los argumentos correspondientes
(define (fix args ids env [acum (list)])
  (match ids
    [(cons (list 'lazy a) b)
     (fix (cdr args) b env (cons (exprv (first args) env (box #f)) acum))]
    [(cons a b)
     (fix (cdr args) b env (cons (interp (first args) env) acum))]
    [(list) (reverse (append (map (\lambda(v) (interp v env)) args) acum))]))

;fix-ids :: list -> list
;Elimina el keyword lazy de los argumentos correspondientes
(define (fix-ids ids [acum (list)])
  (match ids
    [(cons (list 'lazy a) b)
           (fix-ids b (cons a acum))]
    [(cons a b) (fix-ids b (cons a acum))]
    [(list) (reverse acum)]))

; strict:: number/boolean/procedure/Struct/promise [Boolean]-> number/boolean/procedure/Struct
; Calculates corresponding promises, if the flag recursive is set to false, it does not calculate the
; promises in the values of a Structv
(define (strict val [recursive #t])
  (match val
    [(exprv expr env cache)
     (def (box c) cache)
     (if  c
         (begin  ;; value already exists
           c)
         (let  ;; value not yet computed
           ([val (strict (interp expr env) recursive)])
           (set-box! cache val) ; updating cache
           val))]
    [(structV name variant params) (if recursive
                                       (structV name variant (map strict params))
                                       val)]
    [_ val]))

; check-zero :: List[Number] -> Boolean
; checks if a list of numbers has a zero in it
(define (check-zero l)
  (match l
    [(list) #f]
    [(cons h t) (if (equal? h 0)
                    #t
                    (check-zero t))]))

;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x) (strict (env-lookup x env) #f)]
    ; function (notice the meta interpretation)
    [(fun ids body)
     (def fixed-ids (fix-ids ids))
     (list ids
     (λ (arg-vals)
       (interp body (extend-env fixed-ids arg-vals env))))]
    ; application
    [(app fun-expr arg-expr-list)
     (def fun (interp fun-expr env))
     (match fun
       [(list ids lambda)
        (def fixed-args (fix arg-expr-list ids env))
        (lambda fixed-args)]
       [_ (fun
           (map (\lambda(v) (interp v env)) arg-expr-list))])]
    ; primitive application
    [(prim-app prim arg-expr-list)
     (def T-args (map (λ (a) (interp a env)) arg-expr-list))
     (if (and (equal? prim '/) (check-zero T-args))
         (error "/: division by zero")
         (apply (cadr (assq prim *primitives*)) T-args))]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (interp expr env))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (interp body (extend-env (map car alist) (map cdr alist) env))]))

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (symbol=? (structV-name (first v)) name))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  (def varargs (variant-params var))
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
               (list varargs (λ (args) (structV name varname args)))
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v) (symbol=? (structV-variant (first v)) varname))
               env))

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern value))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define (match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     (list #f))]
                [(x y) (error "Match failure")]))



;; run :: s-expr -> number/boolean/procedura/struct
(define(run prog)
  (def prog2 (append `{local {,List ,length}} (list prog)))
  (def result (strict (interp (parse prog2) empty-env)))
  (match result
    [(structV name variant values) (pretty-printing result)]
    [_ result]))

; pretty-printing-list :: struct(list) -> string
; helper function for pretty-printing a struct of the list type
(define (pretty-printing-list s)
  (match s
    [(structV 'List 'Empty values) ""]
    [(structV 'List 'Cons values) (format " ~a~a" (match (first values)
                                                    [(structV name variant values2) (pretty-printing (first values)) ]
                                                    [_ (first values)])
                                                    (string-join (map pretty-printing-list (cdr values))))]))

; pretty-printing :: number/boolean/procedura/struct -> string
; transforms structs to an easier to read format
(define (pretty-printing s)
  (match s
    [(structV 'List variant values) (format "{list~a}" (pretty-printing-list s))]
    [(structV name variant values) (match values
                                     ['() (format "{~a}" variant)]
                                     [(list a ...) (format "{~a ~a}" variant (string-join (map pretty-printing values)))])]
    [_ (format "~a" s)]))

                                                                
#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (apply / args)))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))

; List
; definition of a List
(def List '{datatype List
                     {Empty}
                     {Cons a b}})

; length :: List -> Number
; return the length of a given list
(def length '{define length {fun {l}
                                 {match l
                                   {case {Empty} => 0}
                                   {case {Cons a b} => {+ 1 {length b}}}}}})
; stream-data
; definition for a stream
(def stream-data  '{datatype Stream
                             {S a {lazy b}}})

; make-stream: Any x Any -> Stream
; makes a stream of 2 elements, the second element has lazy evaluation
(def make-stream '{define make-stream
                    {fun {hd {lazy tl}}
                         {S hd tl}}})
; ones
; infinite stream of the number 1
(def ones '{define ones {make-stream 1 ones}})

; stream-hd :: Stream -> Any
; returns the first element of a stream
(def stream-hd '{define stream-hd {fun {s}
                                       {match s
                                         {case {S a b} => a}}}})
; stream-tl :: Stream -> Any
; returns the second element of a stream
(def stream-tl '{define stream-tl {fun {s}
                                     {match s
                                       {case {S a b} => b}}}})

; stream-take :: Number x Stream -> List
; returns a list of the first n elements of a stream
(def stream-take '{define stream-take {fun {n s}
                                           {if {zero? n}
                                               {Empty}
                                               {Cons {stream-hd s} {stream-take {- n 1} {stream-tl s}}}}}})
; stream-lib
; libreary of all the functions for streams
(def stream-lib (list stream-data
                      make-stream
                      stream-hd
                      stream-tl
                      stream-take))

; stream-zipWith :: function Stream Stream -> Stream
; takes a function and two streams, and returns a stream of the elements of both streams with the function appplied to them
(def stream-zipWith '{define stream-zipWith {fun {f s1 s2}
                                                 {make-stream
                                                  {f {stream-hd s1} {stream-hd s2}}
                                                  {stream-zipWith f {stream-tl s1} {stream-tl s2}}}}})
; fib
; fibonacci series as a stream
(def fibs '{define fibs {make-stream 1
                                     {make-stream 1
                                                  {stream-zipWith
                                                   {fun {n m}
                                                        {+ n m}}
                                                   fibs
                                                   {stream-tl fibs}}}}})

; merge-sort :: Stream Stream -> Stream
; takes two sorted streams, and returns the sorted stream of the elements of both streams
(def merge-sort '{define merge-sort {fun {s1 s2}
                                         {if {<=
                                              {stream-hd s1}
                                              {stream-hd s2}}
                                             {make-stream {stream-hd s1} {merge-sort
                                                                          {stream-tl s1}
                                                                          s2}}
                                             {make-stream {stream-hd s2} {merge-sort
                                                                          s1
                                                                          {stream-tl s2}}}}}})

