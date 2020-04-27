#lang racket
(provide (all-defined-out))

;----------------------------------------------------------------------;

; By Kienan Ahner-McHaffie (kta12)
; and Anna Dutkiewicz (amd219)

;----------------------------------------------------------------------;
; STATE

; Get the name of the given state entry
; Test case: (state_name '(y 7))
;         -> 'y
(define state_name car)

; Get the value of the given state entry
; Test case: (state_value '(y 7))
;         -> 7
(define state_value cadr)

; Get the next state entry
; Test case: (state_current '((y 7) (x 5) (z 3)))
;         -> '(y 7)
(define state_current car)

; Get a list of all state entries after the next one
; Test case: (state_remaining '((y 7) (x 5) (z 3)))
;         -> '((x 5) (z 3))
(define state_remaining cdr)

;----------------------------------------------------------------------;
; LAYERS

; Get the top state layer
; Test case: (layer_current '(((a 6) (b 10) (c 8)) ((y 7) (x 5) (z 3))))
;         -> '((a 6) (b 10) (c 8))
(define layer_current car)

; Get a list of all state layers after the next one
; Test case: (layer_remaining '(((y 7) (x 5)) ((z 3))))
;         -> '(((z 3)))
(define layer_remaining cdr)

;----------------------------------------------------------------------;
; ISDEC

; Returns #t iff 'name' is declared in 'state'.
; This checks whether a variable name is in scope at all.
; For example, if a variable is is global scope with name 'x', isdec on 'x' will be #t.
; Test case: (state_isdec 'x '(((x 2) (y 3))))
;         -> #t
; Test case: (state_isdec 'z '(((x 2) (y 3))))
;         -> #f
; Test case: (state_isdec 'a '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #t
; Test case: (state_isdec 'x '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #t
; Test case: (state_isdec 'z '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #f
; Test case: (state_isdec 'b '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> #t
(define state_isdec
  (lambda (name state)
    (cond
      ((null?                state )                                 #f                                                                                        )
      ((null? (layer_current state))                                 (state_isdec name (layer_remaining state))                                                )
      ((eq? name (state_name (state_current (layer_current state)))) #t                                                                                        )
      (else                                                          (state_isdec name (cons (state_remaining (layer_current state)) (layer_remaining state))) ) )))

; Returns #t iff 'name' is locally declared in 'state'.
; This checks whether a variable name is in the topmost layer of the state
; For example, if a variable is is global scope with name 'x', isdec on 'x' will be
; #f if we're in a local scope where re-declaring 'x' would be valid.
; Test case: (state_islocaldec 'x '(((x 2) (y 3))))
;         -> #t
; Test case: (state_islocaldec 'z '(((x 2) (y 3))))
;         -> #f
; Test case: (state_islocaldec 'a '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #t
; Test case: (state_islocaldec 'x '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #f
; Test case: (state_islocaldec 'z '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #f
; Test case: (state_islocaldec 'b '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> #t
; Test case: (state_islocaldec 'x '(((i 5)) ((y 7) (x 5) (z 3))))
;         -> #f
(define state_islocaldec
  (lambda (name state)
    (cond
      ((null?                state )                                 #f                                                                                             )
      ((null? (layer_current state))                                 #f                                                                                             )
      ((eq? name (state_name (state_current (layer_current state)))) #t                                                                                             )
      (else                                                          (state_islocaldec name (cons (state_remaining (layer_current state)) (layer_remaining state))) ) )))

;----------------------------------------------------------------------;
; LOOKUP

; Either sends a state to state_lookup or layer_lookup
; Get the value of variable 'name' in state 'state'
; Raises an error if the variable was not declared
; Raises an error if the variable was not assigned
; Test case: (state_lookup 'x '(((x 2) (y 3))))
;         -> 2
; Test case: (state_lookup 'y '(((x 2) (y 3))))
;         -> 3
; Test case: (state_lookup 'z '(((x 2) (y 3))))
;         -> #error#
; Test case: (state_lookup 'y '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> 3
; Test case: (state_lookup 'z '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> #error#
; Test case: (state_lookup 'b '(((a 1) (b 4)) ((x 2) (y 3))))
;         -> 4
; Test case: (state_lookup 'b '(() ((a 1) (b 4)) ((x 2) (y 3))))
;         -> 4
(define state_lookup
  (lambda (name state)
    (cond
      ((null?                state )                                 (error 'declare "Oops, tried to read an un-declared variable!")                            )
      ((null? (layer_current state))                                 (state_lookup name (layer_remaining state))                                                )
      ((eq? name (state_name (state_current (layer_current state)))) (state_value (state_current (layer_current state)))                                        )
      (else                                                          (state_lookup name (cons (state_remaining (layer_current state)) (layer_remaining state))) ) )))

;----------------------------------------------------------------------;
; DECLARE

; Returns a state with the given name declared
; Should the given name already be declared, an error is raised.
; Declared names start with value 'error.
; This ensures that referencing them before they're assigned is an error.
; Test case: (state_declare 'w '(((y 7) (x 5) (z 3))))
;         -> '(((w error) (y 7) (x 5) (z 3)))
; Test case: (state_declare 'w '(((y 7) (x 5)) ((z 3))))
;         -> '(((w error) (y 7) (x 5)) ((z 3)))
; Test case: (state_declare 'x '(((y 7) (x 5) (z 3))))
;         -> #error#
; Test case: (state_declare 'x '(((y 7)) ((x 5) (z 3))))
;         -> #error#
; Test case: (state_declare 'c '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> '(((c error) (a 1) (b 4)) ((y 7) (x 5) (z 3)))
(define state_declare
  (lambda (name state)
    (cond
      ((null? state)                 (list (list name 'error))                                                      )
      ((state_islocaldec name state) (error 'declare "Oops, a variable was redeclared!")                            )
      (else                          (cons (cons (list name 'error) (layer_current state)) (layer_remaining state)) ) )))

;----------------------------------------------------------------------;
; UPDATE

; Returns a state with the given name's associated value updated to the given value.
; An error is raised if the name was not previously declared with 'state_declare'.
; Only the first layer found with the given name is updated; If the name is found in multiple layers it is only updated in one.
; Test case: (state_update 'x 3 '(((y 7) (x 5) (z 3))))
;         -> '(((y 7) (x 3) (z 3)))
; Test case: (state_update 'w 5 '(((y 7) (x 5) (z 3))))
;         -> #error#
; Test case: (state_update 'x 3 '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> '(((a 1) (b 4)) ((y 7) (x 3) (z 3)))
; Test case: (state_update 'b 2 '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> '(((a 1) (b 2)) ((y 7) (x 3) (z 3)))
; Test case: (state_update 'w 5 '(((a 1) (b 4)) ((y 7) (x 5) (z 3))))
;         -> #error#
; Test case: (state_update 'b 2 '(((a 1) (b 4)) ((y 7) (x 5) (b 6))))
;         -> '(((a 1) (b 2)) ((y 7) (x 3) (b 6)))
(define state_update
  (lambda (name value state)
    (cond
      ((null? state)                 (error 'declare "Oops, tried to write to an un-declared variable!")                       )
      ((state_islocaldec name state) (cons (state_update_impl_layer name value (layer_current state)) (layer_remaining state)) )
      (else                          (cons (layer_current state) (state_update name value (layer_remaining state)))            ) )))

; Helper function that performs state_update with no checks on each item in the given layer
(define state_update_impl_layer
  (lambda (name value layer)
    (cond
      ((null? layer)                                 '()                                                                                       )
      ((eq? name (state_name (state_current layer))) (cons (list name value) (state_remaining layer))                                          )
      (else                                          (cons (state_current layer) (state_update_impl_layer name value (state_remaining layer))) ) )))

;----------------------------------------------------------------------;
; LAYERS

; Adds an empty layer onto the state
; Test case: (pushlayer '())
;         -> '(())
; Test case: (pushlayer '(()))
;         -> '(() ())
; Test case: (pushlayer '(((y 7) (x 5) (z 3))))
;         -> '(() ((y 7) (x 5) (z 3)))
; Test case: (pushlayer '(((i 1) (j 6)) ((y 7) (x 5) (z 3))))
;         -> '(() ((i 1) (j 6)) ((y 7) (x 5) (z 3)))
(define pushlayer
  (lambda (state)
    (cons '() state)))

; Removes the top layer from the state
; Test case: (poplayer '(((a 6) (b 10) (c 8))))
;         -> '()
; Test case: (poplayer '(() ((y 7) (x 5) (z 3))))
;         -> '(((y 7) (x 5) (z 3)))
; Test case: (poplayer '(((a 6) (b 10) (c 8)) ((y 7) (x 5) (z 3))))
;         -> '(((y 7) (x 5) (z 3)))
; Test case: (poplayer '(((a 6) (b 10) (c 8)) ((i 1) (j 6)) ((y 7) (x 5) (z 3))))
;         -> '(((i 1) (j 6)) ((y 7) (x 5) (z 3)))
(define poplayer
  (lambda (state)
    (state_remaining state)))

;----------------------------------------------------------------------;
; FUNCTION-VISIBLE

; Combines a state that was invisible to a function with a state that was visible to a function
; If neither invisible nor visible state was changed, the result should be the same as the input used to create the invisible and visible state
; Test case: (state_combineinvisiblevisible '(((a 1) (b 2)) ((c 3) (d 4))) '(((e 5) (f 6)) ((g 7) (h 8))))
;         -> '(((a 1) (b 2)) ((c 3) (d 4) (e 5) (f 6)) ((g 7) (h 8)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'z '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'z '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'a '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'a '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'b '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'b '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'e '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'e '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'c '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'c '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
; Test case: (state_combineinvisiblevisible (state_functioninvisible 'n '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))) (state_functionvisible 'n '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))))
;         -> '(((z 2)) ((a 1) (b 2) (e 2) (c 3)) ((n 3)))
(define state_combineinvisiblevisible
  (lambda (state_inv state_vis)
    (cond
      ((null? (layer_remaining state_inv)) (cons (append (layer_current state_inv) (layer_current state_vis)) (layer_remaining state_vis)) )
      (else                                (cons (layer_current state_inv) (state_combineinvisiblevisible (layer_remaining state_inv) state_vis)) ) )))

; Returns a state containing all state members that are invisible to the function with the given name
; Note that if the layer containing the function has no visible members, it is included as an empty layer
; Note that the topmost two layers are never invisible to anyone, as they are global
; Test case: (state_functioninvisible 'abc '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3)) ((x 5))))
;         -> '(((z 2)) ((a 1) (b 2)))
; Test case: (state_functioninvisible 'a '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3)) ((x 5))))
;         -> '(((z 2)) ())
; Test case: (state_functioninvisible 'c '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)) ((x 5))))
;         -> '(((z 2)) ((a 1) (b 2) (abc 2)))
; Test case: (state_functioninvisible 'n '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)) ((x 5))))
;         -> '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ())
; Test case: (state_functioninvisible 'g '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)) ((x 5))))
;         -> '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ())
; Test case: (state_functioninvisible 'x '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)) ((x 5))))
;         -> '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ())
(define state_functioninvisible
  (lambda (name state)
    (cond
      ((or (null? (layer_remaining state)) (null? (layer_remaining (layer_remaining state)))) '(())                                                                               )
      ((state_islocaldec name state)                                                          (list (state_functioninvisible_layerimpl name (layer_current state)))               )
      (else                                                                                   (cons (layer_current state) (state_functioninvisible name (layer_remaining state))) ) )))

; Internal implementaion of 'state_functioninvisible' that operates on a single layer known to contain the given function
; Test case: (state_functioninvisible_layerimpl 'abc '((a 1) (b 2) (abc 2) (c 3)))
;         -> '((a 1) (b 2))
(define state_functioninvisible_layerimpl
  (lambda (name layer)
    (cond
      ((null? layer)                                 (error 'nofunc "Could not find function in layer!")                                           )
      ((eq? name (state_name (state_current layer))) '()                                                                                           )
      (else                                          (cons (state_current layer) (state_functioninvisible_layerimpl name (state_remaining layer))) ) )))

; Returns a state containing all state members that are visible to the function with the given name
; Note that this includes the function itself, so it is impossible for the function's own layer to have no visible members
; Note that the topmost two layers are always visible to everyone, as they are global
; Test case: (state_functionvisible 'x '(((z 2) (x 3)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4))))
;         -> '(((x 3)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)))
; Test case: (state_functionvisible 'g '(((z 2)) ((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4))))
;         -> '(((a 1) (b 2) (abc 2) (c 3)) ((n 3) (g 4)))
(define state_functionvisible
  (lambda (name state)
    (cond
      ((or (null? (layer_remaining state)) (null? (layer_remaining (layer_remaining state)))) state                                                                                               )
      ((null? (layer_current state))                                                          (state_functionvisible name (layer_remaining state))                                                )
      ((eq? name (state_name (state_current (layer_current state))))                          state                                                                                               )
      (else                                                                                   (state_functionvisible name (cons (state_remaining (layer_current state)) (layer_remaining state))) ) )))

