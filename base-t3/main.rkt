#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  (object target members)
  (get id)
  (set id e)
  (send obj met vals)
  (this)
  (shallow-copy expr)
  (deep-copy expr))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (ObjectV target members oenv))

;; Members
(deftype Member
  (field id e)
  (method id vals body))

(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) -(error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    ['this (this)]
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    [(list 'object ': target e ...)
     (object (parse target) (map parse-member e))]
    [(list 'object e ...) (object #f (map parse-member e))]    
    [(list 'get id) (get (string->symbol(string-append "f" (symbol->string id))))]
    [(list 'set id e) (set (string->symbol(string-append "f" (symbol->string id))) (parse e))]
    [(list 'send ob met val ...) (send (parse ob) (string->symbol(string-append "m" (symbol->string met))) (map parse val))]
    [(list 'shallow-copy expr) (shallow-copy (parse expr))]
    [(list 'deep-copy expr) (deep-copy (parse expr))]
    [(list 'fun vals expr) (object #f (list (parse-member (list 'method 'f vals expr))))]
    [(list e ...) (send (parse (first e)) 'mf (map parse (cdr e)))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; parse-members :: s-expr -> Member
;; parses a member to it's corresponding type, it adds "f" or "m" to the symbol to differentiate between fields and objects
(define (parse-member e)
  (match e
    [(list 'field id e)
     (if (equal? 'this id)
         (error "this is a reserved word")
         (field
          (string->symbol(string-append "f" (symbol->string id)))
          (parse e)))]
    [(list 'method id (list vals ...) body)
     (if (equal? 'this id)
         (error "this is a reserved word")
         (method
          (string->symbol(string-append "m" (symbol->string id)))
          vals
          (parse body)))]))

;; interp :: Expr Env -> Val
(define (interp expr env [inObject #f])
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r)
     (make-val (f (open-val (interp l env inObject))
                  (open-val (interp r env inObject))))]
    [(unop f s) (make-val (f (open-val (interp s env inObject))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env inObject))
     (if cnd
         (interp t env inObject)
         (interp f env inObject))]
    [(id x) (env-lookup x env)]
    [(seqn expr1 expr2) (begin                          
                          (interp expr1 env inObject)                          
                          (interp expr2 env inObject))]
    [(object target members)(ObjectV
                             (if target
                                 (interp target env)
                                 target)
                             (box (interp-members members env))
                             env)]
    [(send ob-id m-id vals)(def (ObjectV target members oenv) (interp ob-id env inObject))
                           (def (cons (cons mid (cons args body)) fobject)
                             (find-method m-id (ObjectV target members oenv)))
                           (def soenv
                             (multi-extend-env
                              (cons 'self args)
                              (cons (ObjectV target members oenv) (map (curryr interp env inObject) vals))
                              oenv))
                           (interp body soenv fobject)]
    [(get id)(if inObject                
                 (let [(found (assoc id (unbox (ObjectV-members inObject))))]
                   (if found
                       (cdr found)
                       (error "field not found")))
                 (error "get used outside of an object"))]
    [(set x e) (if inObject
                   (let [(members (unbox (ObjectV-members inObject)))]
                     (let [(found (assoc x members))]
                       (if found
                           (let [(new-members (append (list (cons x (interp e env inObject))) members))]
                             (set-box! (ObjectV-members inObject) new-members))
                           (error "field not found"))))
                   (error "set used outside of an object"))]
    [(this) (if inObject
                (env-lookup 'self env)
                (error "this must be used inside an object"))]
    [(shallow-copy e) (def (ObjectV target members oenv) (interp e env inObject))
                      (ObjectV target (box (unbox members)) oenv)]
    [(deep-copy e) (def object (interp e env inObject))
                   (copy-in-deep object)]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (interp body new-env))
     ]))

; find-method :: Symbol ObjectV -> (method ObjectV)
; this methods finds a method identifier in the list of members of the given ObjectV
; returns the method and the ObjectV where the method was found
(define (find-method m-id object)
  (match object
    [(ObjectV target members oenv) (def found (assoc m-id (unbox members)))
                                   (if found
                                       (cons found object)
                                       (if target
                                           (find-method m-id target)
                                           (error "message not understood")))]))

; copy-in-deep :: ObjectV -> ObjectV
; returns a deep-copy of the given object
(define (copy-in-deep object)
  (match object
    [(ObjectV target members oenv) (if target
                                       (ObjectV (copy-in-deep target) (box (unbox members)) oenv)
                                       (ObjectV #f (box (unbox members)) oenv))]))

; interp-members :: List(members) aEnv -> List
; this function takes a list of members and a enviroment, and returns a dictionary of
; the members, asociating their identifier with their meaning
(define (interp-members members env [acum '()])
  (match members
    [(cons h t) (match h
                  [(field id e)(interp-members t env (cons (cons id (interp e env)) acum))]
                  [(method id vals body) (interp-members t env (cons (cons id (cons vals body)) acum))])]
    [empty acum]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))