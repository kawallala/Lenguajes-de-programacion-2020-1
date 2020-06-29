#lang play
{with {x 5} {+ x x}}

subst x por 5 en
{+ x x} ---> {+ 5 5}

{+ 1 2} ---> {+ 1 2}

{+ x {with {x 3} 10}} --- > {+ 5 {with {5 3} 10}}

;binding instance (instancia de enlace)
;{with {x ...} ...}
;cuando se introduce un identificador

{with {x 1} {+ x x}}

;alcance de un identificador
;region del programa donde las ocurrencias del identificador
;se refieren al valor asociado

{+ {with {x 1} {+ x x}} x}

;bound identifier (identificador enlazados)
;free identifier (identificador libre)

;Def 2: Def1 salvo para instancias de enlace

{+ x {with {x 3} 10}} --- > {+ 5 {with {x 3} 10}}

{+ x {with {x 3} x}} --- > {+ 5 {with {x 3} 5}}

;Def 3 Def2 excepto en alcances anidados

{+ x {with {x 3} x}} --- > {+ 5 {with {x 3} x}}

{+ x {with {y 3} {+ x y}}} --- > {+ 5 {with {y 3} {+ x y}}}

;Def 4: Def 2 excepto en alcances anidados del mismo identificador

{+ x {with {y 3} {+ x y}}} --- > {+ 5 {with {y 3} {+ 5 y}}}

;Def 4:= para substituir x por v en e, hay que reemplazar las ocurrenncias
;de x salvo las de binding, y salvo las que estan en alcances anidados de x

{+ x {with {y {+ x 3}}
           {with {x 10}
                 {+ x y}}}}
;Def4 := para substituir x por v en e, hay que reemplazar las ocurrenncias
; libres de x en e por v.