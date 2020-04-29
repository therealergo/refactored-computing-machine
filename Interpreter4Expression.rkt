#lang racket
(provide (all-defined-out))
(require "Interpreter4State.rkt")

;----------------------------------------------------------------------;

; By Kienan Ahner-McHaffie (kta12)
; and Anna Dutkiewicz (amd219)

;----------------------------------------------------------------------;
; EXPRESSION

; What should the given expression actually *do*?
; Test case: (expression_operator '(+ y (+ y 1)))
;         -> '+
(define expression_operator car)

; Is there even an expression operator to test?
; Test case: (expression_hasoperator '(+ y (+ y 1)))
;         -> #t
; Test case: (expression_hasoperator 'x)
;         -> #f
(define expression_hasoperator
  (lambda (expression)
    (list? expression)))

; What's the first value given to the expression?
; Test case: (expression_operand1 '(+ y (+ y 1)))
;         -> 'y
(define expression_operand1 cadr)

; What's the second value given to the expression?
; Test case: (expression_operand2 '(+ y (+ y 1)))
;         -> '(+ y 1)
(define expression_operand2 caddr)

; Was there a second value given to the expression?
; Test case: (expression_hasOperand2 '(- y (+ y 1)))
;         -> #t
; Test case: (expression_hasOperand2 '(- y)
;         -> #f
(define expression_hasOperand2
  (lambda (declaration)
    (not (null? (cddr declaration)))))

;----------------------------------------------------------------------;

; What should the given conditional actually *do*
; Test case: (expression_operator '(== x y))
;         -> '==
(define conditional_operator car)

; Is there even a conditional operator to test?
; Test case: (conditional_hasoperator '(! y))
;         -> #t
; Test case: (conditional_hasoperator 'x)
;         -> #f
(define conditional_hasoperator
  (lambda (expression)
    (list? expression)))

; What's the first value given to the conditional?
; Test case: (conditional_operand1 '(== "true" y))
;         -> "true"
(define conditional_operand1 cadr)

; What's the second value given to the conditional?
; Test case: (conditional_operand2 '(== "true" y))
;         -> 'y
(define conditional_operand2 caddr)

;----------------------------------------------------------------------;

; Convert a boolean value from our interpreter into a racket-usable boolean value
; Test case: (boolean_toRacket 'true)
;         -> #t
; Test case: (boolean_toRacket 'false)
;         -> #f
; Test case: (boolean_toRacket 7)
;         -> #error#
; Test case: (boolean_toRacket 'bananas)
;         -> #error#
(define boolean_to_racket
  (lambda (boolean)
    (cond
      ((eq? boolean 'true ) #t)
      ((eq? boolean 'false) #f)
      (else (error 'type "Oops, value is not boolean type!")))))

; Convert a boolean value from racket into a value understood by our interpreter
; Test case: (boolean_toInterpret #t)
;         -> "true"
; Test case: (boolean_toInterpret #f)
;         -> "false"
; Test case: (boolean_toInterpret 7)
;         -> #error#
; Test case: (boolean_toInterpret 'bananas)
;         -> #error#
(define boolean_to_interpret
  (lambda (boolean)
    (cond
      ((eq? boolean #t) 'true )
      ((eq? boolean #f) 'false)
      (else (error 'type "Oops, value is not boolean type!")))))

;----------------------------------------------------------------------;

; Evaluate the given expression against the given state
; This one's a bit ugly running off of the edge of the screen --
; I'm going to blame the *extra challenge* part mixed with my long function names for that.
; Note: f_o, f_v, and f_s are abbreviations for funcall_operator, funcall_resultvalue, and funcall_resultstate respectively.
;       These are the the functions used to execute any expression that starts with 'funcall.
; Test case: (expression_value 'f_o 'f_v 'f_s '(+ 1 (/ 3 2)) '())
;         -> 2
; Test case: (expression_value 'f_o 'f_v 'f_s '(- (/ 3 2)) '())
;         -> -1
; Test case: (expression_value 'f_o 'f_v 'f_s '(== 1 1) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(== 1 2) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(== true true) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(== true false) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(&& true false) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(&& true true) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(|| true false) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(|| false false) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(>= 1 2) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(>= 2 2) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(! true) '())
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(! 'false) '())
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(== x y) '((x 2) (y 3)))
;         -> 'false
; Test case: (expression_value 'f_o 'f_v 'f_s '(== x y) '((x 3) (y 3)))
;         -> 'true
; Test case: (expression_value 'f_o 'f_v 'f_s '(= x 2) '())
;         -> 2
; Test case: (expression_value 'f_o 'f_v 'f_s '(= x (+ x 5)) '((x 7)))
;         -> 12
(define expression_value
  (lambda (f_o f_v f_s in state)
    (cond
      ((null? in) (error 'parse "Oops, no expression given!"))
      ((number? in) in)
      ((eq? in 'true ) in)
      ((eq? in 'false) in)
      ((state_isdec in state) (state_lookup in state))
      ((not (or (expression_hasoperator in) (conditional_hasoperator in))) (error 'declare "Oops, attempted expression with un-declared variable!"))
      ((and (eq? '- (expression_operator in)) (not (expression_hasOperand2 in))) (-        (expression_value f_o f_v f_s (expression_operand1 in) state)                                                                                               ))
      (     (eq? '+ (expression_operator in))                                    (+        (expression_value f_o f_v f_s (expression_operand1 in) state) (expression_value f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))
      (     (eq? '- (expression_operator in))                                    (-        (expression_value f_o f_v f_s (expression_operand1 in) state) (expression_value f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))
      (     (eq? '* (expression_operator in))                                    (*        (expression_value f_o f_v f_s (expression_operand1 in) state) (expression_value f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))
      (     (eq? '/ (expression_operator in))                                    (quotient (expression_value f_o f_v f_s (expression_operand1 in) state) (expression_value f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))
      (     (eq? '% (expression_operator in))                                    (modulo   (expression_value f_o f_v f_s (expression_operand1 in) state) (expression_value f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))
      ((eq? '== (conditional_operator in)) (boolean_to_interpret (eq?      (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))  )))
      ((eq? '!= (conditional_operator in)) (boolean_to_interpret (not (eq? (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) ))))
      ((eq? '<  (conditional_operator in)) (boolean_to_interpret (<        (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))  )))
      ((eq? '>  (conditional_operator in)) (boolean_to_interpret (>        (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))  )))
      ((eq? '<= (conditional_operator in)) (boolean_to_interpret (<=       (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))  )))
      ((eq? '>= (conditional_operator in)) (boolean_to_interpret (>=       (expression_value f_o f_v f_s (conditional_operand1 in) state) (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))  )))
      ((eq? '&& (conditional_operator in)) (boolean_to_interpret (and      (boolean_to_racket (expression_value f_o f_v f_s (conditional_operand1 in) state)) (boolean_to_racket (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))) )))
      ((eq? '|| (conditional_operator in)) (boolean_to_interpret (or       (boolean_to_racket (expression_value f_o f_v f_s (conditional_operand1 in) state)) (boolean_to_racket (expression_value f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state))) )))
      ((eq? '!  (conditional_operator in)) (boolean_to_interpret (not      (boolean_to_racket (expression_value f_o f_v f_s (conditional_operand1 in) state))                                                                                                                    )))
      ((eq? '=  (conditional_operator in)) (expression_value f_o f_v f_s (expression_operand2 in) state))
      ((eq? 'funcall (f_o in)) (f_v (f_s in state)))
      (else (error 'badop "Oops, bad expression given!")))))

; Get the resulting state after evaluating the given expression against the given state.
; Note: f_o, f_v, and f_s are abbreviations for funcall_operator, funcall_resultvalue, and funcall_resultstate respectively.
;       These are the the functions used to execute any expression that starts with 'funcall.
; Test case: (expression_state 'f_o 'f_v 'f_s '(= x (+ x 5)) '((x 7)))
;         -> '((x 12))
(define expression_state
  (lambda (f_o f_v f_s in state)
    (cond
      ((null? in) (error 'parse "Oops, no expression given!"))
      ((number? in)           state)
      ((eq? in 'true )        state)
      ((eq? in 'false)        state)
      ((state_isdec in state) state)
      ((and (eq? '- (expression_operator in)) (not (expression_hasOperand2 in))) (expression_state f_o f_v f_s (expression_operand1 in) state                                            ) )
      (     (eq? '+ (expression_operator in))                                    (expression_state f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) )
      (     (eq? '- (expression_operator in))                                    (expression_state f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) )
      (     (eq? '* (expression_operator in))                                    (expression_state f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) )
      (     (eq? '/ (expression_operator in))                                    (expression_state f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) )
      (     (eq? '% (expression_operator in))                                    (expression_state f_o f_v f_s (expression_operand2 in) (expression_state f_o f_v f_s (expression_operand1 in) state)) )
      ((eq? '== (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '!= (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '<  (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '>  (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '<= (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '>= (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '&& (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '|| (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand2 in) (expression_state f_o f_v f_s (conditional_operand1 in) state)) )
      ((eq? '!  (conditional_operator in)) (expression_state f_o f_v f_s (conditional_operand1 in) state                                             ) )
      ((eq? '=  (expression_operator in )) (state_update                 (conditional_operand1 in) (expression_value f_o f_v f_s (conditional_operand2 in) state) (expression_state f_o f_v f_s (conditional_operand2 in) state)))
      ((eq? 'funcall (f_o in)) (f_s in state))
      (else (error 'badop "Oops, bad expression given!")))))
