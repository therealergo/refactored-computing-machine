#lang racket
(provide (all-defined-out))
(require "functionParser.rkt")
(require "Interpreter3State.rkt")
; Testing that I can, in fact, successfully push changes to git before I make any important changes

;----------------------------------------------------------------------;

; By Kienan Ahner-McHaffie (kta12)
; and Anna Dutkiewicz (amd219)
; It's an interpreter!
; Functions are all named with the following convention:
; (PRIMARY INPUT)_(OUTPUT)
; So for example, "return_expression" is a function that takes in a
; return statement and outputs the expression part of that return statement.
; Test cases are shown throughout to demonstrate the functionality of each function.
; Here's some examples of how to use this interpreter, in the form of test cases:

; TEST
; Test case: (interpret "Programs/test1.javacish")
;         -> 10
; Test case: (interpret "Programs/test2.javacish")
;         -> 14
; Test case: (interpret "Programs/test3.javacish")
;         -> 45
; Test case: (interpret "Programs/test4.javacish")
;         -> 55
; Test case: (interpret "Programs/test5.javacish")
;         -> 1
; Test case: (interpret "Programs/test6.javacish")
;         -> 115
; Test case: (interpret "Programs/test7.javacish")
;         -> 'true
; Test case: (interpret "Programs/test8.javacish")
;         -> 20
; Test case: (interpret "Programs/test9.javacish")
;         -> 24
; Test case: (interpret "Programs/test10.javacish")
;         -> 2
; Test case: (interpret "Programs/test11.javacish")
;         -> 35
; Test case: (interpret "Programs/test12.javacish")
;         -> #error#
; Test case: (interpret "Programs/test13.javacish")
;         -> 90
; Test case: (interpret "Programs/test14.javacish")
;         -> 69
; Test case: (interpret "Programs/test15.javacish")
;         -> 87
; Test case: (interpret "Programs/test16.javacish")
;         -> 64
; Test case: (interpret "Programs/test17.javacish")
;         -> #error#
; Test case: (interpret "Programs/test18.javacish")
;         -> 125
; Test case: (interpret "Programs/test19.javacish")
;         -> 100
; Test case: (interpret "Programs/test20.javacish")
;         -> 2000400

; CHECK
; Test case: (interpret "Programs/check1.javacish")
;         -> 10
; Test case: (interpret "Programs/check2.javacish")
;         -> 44
; Test case: (interpret "Programs/check3.javacish")
;         -> 44
; Test case: (interpret "Programs/check4.javacish")
;         -> 44
; Test case: (interpret "Programs/check5.javacish")
;         -> 45
; Test case: (interpret "Programs/check6.javacish")
;         -> 100
; Test case: (interpret "Programs/check7.javacish")
;         -> 100
; Test case: (interpret "Programs/check8.javacish")
;         -> #error#
; Test case: (interpret "Programs/check9.javacish")
;         -> #error#
; Test case: (interpret "Programs/check10.javacish")
;         -> #error#
; Test case: (interpret "Programs/check11.javacish")
;         -> 7
; Test case: (interpret "Programs/check12.javacish")
;         -> 5
; Test case: (interpret "Programs/check13.javacish")
;         -> 10
; Test case: (interpret "Programs/checka.javacish")
;         -> -1
; Test case: (interpret "Programs/checkb.javacish")
;         -> 0
; Test case: (interpret "Programs/checkc.javacish")
;         -> 362880000
; Test case: (interpret "Programs/checkd.javacish")
;         -> 2
; Test case: (interpret "Programs/checke.javacish")
;         -> 7

;----------------------------------------------------------------------;

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
; Test case: (expression_value '(+ 1 (/ 3 2)) '())
;         -> 2
; Test case: (expression_value '(- (/ 3 2)) '())
;         -> -1
; Test case: (expression_value '(== 1 1) '())
;         -> 'true
; Test case: (expression_value '(== 1 2) '())
;         -> 'false
; Test case: (expression_value '(== true true) '())
;         -> 'true
; Test case: (expression_value '(== true false) '())
;         -> 'false
; Test case: (expression_value '(&& true false) '())
;         -> 'false
; Test case: (expression_value '(&& true true) '())
;         -> 'true
; Test case: (expression_value '(|| true false) '())
;         -> 'true
; Test case: (expression_value '(|| false false) '())
;         -> 'false
; Test case: (expression_value '(>= 1 2) '())
;         -> 'false
; Test case: (expression_value '(>= 2 2) '())
;         -> 'true
; Test case: (expression_value '(! true) '())
;         -> 'false
; Test case: (expression_value '(! 'false) '())
;         -> 'true
; Test case: (expression_value '(== x y) '((x 2) (y 3)))
;         -> 'false
; Test case: (expression_value '(== x y) '((x 3) (y 3)))
;         -> 'true
; Test case: (expression_value '(= x 2) '())
;         -> 2
; Test case: (expression_value '(= x (+ x 5)) '((x 7)))
;         -> 12
(define expression_value
  (lambda (in state)
    (cond
      ((null? in) (error 'parse "Oops, no expression given!"))
      ((number? in) in)
      ((eq? in 'true ) in)
      ((eq? in 'false) in)
      ((state_isdec in state) (state_lookup in state))
      ((not (or (expression_hasoperator in) (conditional_hasoperator in))) (error 'declare "Oops, attempted expression with un-declared variable!"))
      ((and (eq? '- (expression_operator in)) (not (expression_hasOperand2 in))) (-        (expression_value (expression_operand1 in) state)                                                                                               ))
      (     (eq? '+ (expression_operator in))                                    (+        (expression_value (expression_operand1 in) state) (expression_value (expression_operand2 in) (expression_state (expression_operand1 in) state)) ))
      (     (eq? '- (expression_operator in))                                    (-        (expression_value (expression_operand1 in) state) (expression_value (expression_operand2 in) (expression_state (expression_operand1 in) state)) ))
      (     (eq? '* (expression_operator in))                                    (*        (expression_value (expression_operand1 in) state) (expression_value (expression_operand2 in) (expression_state (expression_operand1 in) state)) ))
      (     (eq? '/ (expression_operator in))                                    (quotient (expression_value (expression_operand1 in) state) (expression_value (expression_operand2 in) (expression_state (expression_operand1 in) state)) ))
      (     (eq? '% (expression_operator in))                                    (modulo   (expression_value (expression_operand1 in) state) (expression_value (expression_operand2 in) (expression_state (expression_operand1 in) state)) ))
      ((eq? '== (conditional_operator in)) (boolean_to_interpret (eq?      (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))  )))
      ((eq? '!= (conditional_operator in)) (boolean_to_interpret (not (eq? (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state)) ))))
      ((eq? '<  (conditional_operator in)) (boolean_to_interpret (<        (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))  )))
      ((eq? '>  (conditional_operator in)) (boolean_to_interpret (>        (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))  )))
      ((eq? '<= (conditional_operator in)) (boolean_to_interpret (<=       (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))  )))
      ((eq? '>= (conditional_operator in)) (boolean_to_interpret (>=       (expression_value (conditional_operand1 in) state) (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))  )))
      ((eq? '&& (conditional_operator in)) (boolean_to_interpret (and      (boolean_to_racket (expression_value (conditional_operand1 in) state)) (boolean_to_racket (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))) )))
      ((eq? '|| (conditional_operator in)) (boolean_to_interpret (or       (boolean_to_racket (expression_value (conditional_operand1 in) state)) (boolean_to_racket (expression_value (conditional_operand2 in) (expression_state (expression_operand1 in) state))) )))
      ((eq? '!  (conditional_operator in)) (boolean_to_interpret (not      (boolean_to_racket (expression_value (conditional_operand1 in) state))                                                                                                                    )))
      ((eq? '=  (conditional_operator in)) (expression_value (expression_operand2 in) state))
      ((eq? 'funcall (statement_operator in)) (funcall_resultvalue (funcall_resultstate in state)))
      (else (error 'badop "Oops, bad expression given!")))))

; Get the resulting state after evaluating the given expression against the given state
; Test case: (expression_state '(= x (+ x 5)) '((x 7)))
;         -> '((x 12))
(define expression_state
  (lambda (in state)
    (cond
      ((null? in) (error 'parse "Oops, no expression given!"))
      ((number? in)           state)
      ((eq? in 'true )        state)
      ((eq? in 'false)        state)
      ((state_isdec in state) state)
      ((and (eq? '- (expression_operator in)) (not (expression_hasOperand2 in))) (expression_state (expression_operand1 in) state                                            ) )
      (     (eq? '+ (expression_operator in))                                    (expression_state (expression_operand2 in) (expression_state (expression_operand1 in) state)) )
      (     (eq? '- (expression_operator in))                                    (expression_state (expression_operand2 in) (expression_state (expression_operand1 in) state)) )
      (     (eq? '* (expression_operator in))                                    (expression_state (expression_operand2 in) (expression_state (expression_operand1 in) state)) )
      (     (eq? '/ (expression_operator in))                                    (expression_state (expression_operand2 in) (expression_state (expression_operand1 in) state)) )
      (     (eq? '% (expression_operator in))                                    (expression_state (expression_operand2 in) (expression_state (expression_operand1 in) state)) )
      ((eq? '== (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '!= (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '<  (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '>  (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '<= (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '>= (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '&& (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '|| (conditional_operator in)) (expression_state (conditional_operand2 in) (expression_state (conditional_operand1 in) state)) )
      ((eq? '!  (conditional_operator in)) (expression_state (conditional_operand1 in) state                                             ) )
      ((eq? '=  (expression_operator in )) (state_update     (conditional_operand1 in) (expression_value (conditional_operand2 in) state) (expression_state (conditional_operand2 in) state)))
      ((eq? 'funcall (statement_operator in)) (funcall_resultstate in state))
      (else (error 'badop "Oops, bad expression given!")))))

;----------------------------------------------------------------------;
; STATEMENT

; What should the given statement actually *do*?
; Test case: (statement_operator '(var x))
;         -> 'var
(define statement_operator car)

;----------------------------------------------------------------------;
; DECLARATION

; Get the 'variable' part of the given declaration statement
; Test case: (declaration_variable '(var x))
;         -> 'var
(define declaration_variable cadr)

; Get the 'expression' part of the given declaration statement
; Test case: (declaration_expression '(var x 5))
;         -> '5
(define declaration_expression caddr)

; Return #t if the given declaration has an expression, and #f otherwise
; Test case: (declaration_has_expression '(var x))
;         -> #f
; Test case: (declaration_has_expression '(var x 5))
;         -> #t
(define declaration_has_expression
  (lambda (declaration)
    (not (null? (cddr declaration)))))

; Return the state that results from executing the given declaration statement
; Test case: (declaration_resultstate '(var x) '(()))
;         -> '(((x error)))
; Test case: (declaration_resultstate '(var x 2) '(()))
;         -> '(((x 2)))
; Test case: (declaration_resultstate '(var x) '(((x 4))))
;         -> #error#
; Test case: (declaration_resultstate '(var x 2) '(((x 4))))
;         -> #error#
; Test case: (declaration_resultstate '(var y (= x 3)) '(((x 5))))
;         -> '(((x 3) (y 3)))
(define declaration_resultstate
  (lambda (in state)
    (cond
      ((declaration_has_expression in) (state_update (declaration_variable in) (expression_value (declaration_expression in) state) (state_declare (declaration_variable in) (expression_state (declaration_expression in) state))) )
      (else                                                                                                                         (state_declare (declaration_variable in)                                               state )  ) )))

;----------------------------------------------------------------------;
; ASSIGNMENT

; Get the 'variable' part of the given assignment statement
; Test case: (assignment_variable '(= y (+ y 1)))
;         -> 'y
(define assignment_variable cadr)

; Get the 'expression' part of the given assignment statement
; Test case: (assignment_expression '(= y (+ y 1)))
;         -> '(+ y 1)
(define assignment_expression caddr)

; Return the state that results from executing the given assignment statement
; Test case: (assignment_resultstate '(= x 2) '(()))
;         -> #error#
; Test case: (assignment_resultstate '(= x 2) '(((x error))))
;         -> '(((x 2)))
; Test case: (assignment_resultstate '(= x 2) '(((x 5))))
;         -> '(((x 2)))
; Test case: (assignment_resultstate '(= x (= y 2)) '(((x error) (y error))))
;         -> '(((x 2) (y 2)))
(define assignment_resultstate
  (lambda (in state)
    (state_update (assignment_variable in) (expression_value (assignment_expression in) state) (expression_state (assignment_expression in) state))))

;----------------------------------------------------------------------;
; FUNCTION

; Get the name of the given function
; Test case: (function_name '(function a (x y) ((return (+ x y)))))
;         -> 'a
; Test case: (function_name '(function a (x y) ((return (+ x y))) ((x 5) (y 7))))
;         -> 'a
(define function_name cadr)

; Get the parameters of the given function
; Test case: (function_parameters '(function a (x y) ((return (+ x y)))))
;         -> '(x y)
; Test case: (function_parameters '(function a (x y) ((return (+ x y))) ((x 5) (y 7))))
;         -> '(x y)
(define function_parameters caddr)

; Get the body of the given function
; Test case: (function_body '(function a (x y) ((return (+ x y)))))
;         -> '((return (+ x y)))
; Test case: (function_body '(function a (x y) ((return (+ x y))) ((x 5) (y 7))))
;         -> '((return (+ x y)))
(define function_body cadddr)

; Return the state that results from executing the given function.
; Because of our implementation, this is actually rather simple:
; The entire function body is just placed into the state, mapped to its name.
; Test case: (function_resultstate '(function test (= y (+ x y))) '(((y 0))))
;         -> '(((test (function x (= y (+ x y)))) (y 0)))
(define function_resultstate
  (lambda (in state)
    (state_update (function_name in) in (state_declare (function_name in) state))))

;----------------------------------------------------------------------;
; FUNCALL

; Get the name of the function to be called
; Test case: (funcall_name '(funcall min (+ x y)))
;         -> 'min
; Test case: (funcall_name '(funcall min (+ x y) z w))
;         -> 'min
(define funcall_name cadr)

; Get the list of argument expressions to call the function with
; Test case: (funcall_args '(funcall min (+ x y)))
;         -> '((+ x y))
; Test case: (funcall_args '(funcall min (+ x y) z w))
;         -> '((+ x y) z w)
(define funcall_args cddr)

; Returns the current argument in the given argument list.
(define arg_current    car)

; Returns a list of all args remaining after the current one.
(define args_remaining cdr)

; Returns the current parameter in the given parameter list.
(define parameter_current    car)

; Returns a list of all parameters remaining after the current one.
(define parameters_remaining cdr)

; Test case: (funcall_evalargs_valuelist '((+ 1 2) 3) '(()))
;         -> '(3 3)
; Test case: (funcall_evalargs_valuelist '((+ 1 2) x) '(((x 5))))
;         -> '(3 5)
; Test case: (funcall_evalargs_valuelist '((+ 1 2) (= x 7)) '(((x 5))))
;         -> '(3 7)
; Test case: (funcall_evalargs_valuelist '((+ 1 2) (= x 7) (= x 9)) '(((x 5))))
;         -> '(3 7 9)
; Test case: (funcall_evalargs_valuelist '((+ 1 2) (= x 7) (= x (+ x 2))) '(((x 5))))
;         -> '(3 7 9)
(define funcall_evalargs_valuelist
  (lambda (args state)
    (cond
      ((null? args) '()                                                                                                                                       )
      (else (cons (expression_value (arg_current args) state) (funcall_evalargs_valuelist (args_remaining args) (expression_state (arg_current args) state))) ) )))

; Test case: (funcall_evalargs_resultstate '((+ 1 2) 3) '(()))
;         -> '(())
; Test case: (funcall_evalargs_resultstate '((+ 1 2) x) '(((x 5))))
;         -> '(((x 5)))
; Test case: (funcall_evalargs_resultstate '((+ 1 2) (= x 7)) '(((x 5))))
;         -> '(((x 7)))
; Test case: (funcall_evalargs_resultstate '((+ 1 2) (= x 7) (= x 9)) '(((x 5))))
;         -> '(((x 9)))
; Test case: (funcall_evalargs_resultstate '((+ 1 2) (= x 7) (= x (+ x 2))) '(((x 5))))
;         -> '(((x 9)))
(define funcall_evalargs_resultstate
  (lambda (args state)
    (cond
      ((null? args) state                                                                                    )
      (else (funcall_evalargs_resultstate (args_remaining args) (expression_state (arg_current args) state)) ) )))

; Test case: (funcall_bindargs2parameters_resultstate '(1 2 3) '(a b c) '(()))
;         -> '(((c 3) (b 2) (a 1)))
; Test case: (funcall_bindargs2parameters_resultstate '(1 2 3) '(a b) '(()))
;         -> #error#
; Test case: (funcall_bindargs2parameters_resultstate '(1 2 3) '(a b c d) '(()))
;         -> #error#
(define funcall_bindargs2parameters_resultstate
  (lambda (args parameters state)
    (cond
      ((and (null? args) (not (null? parameters))) (error 'badparam "Oops, too many parameters!")                                                                                                                                                                          )
      ((and (not (null? args)) (null? parameters)) (error 'badparam "Oops, too many arguments!" )                                                                                                                                                                          )
      ((null? args)                                state                                                                                                                                                                                                                   )
      ((state_islocaldec (arg_current args) state) (error 'badparam "Oops, a parameter was previously defined. Maybe there are multiple parameters with the same name?")                                                                                                   )
      (else                                        (funcall_bindargs2parameters_resultstate (args_remaining args) (parameters_remaining parameters) (state_update (parameter_current parameters) (arg_current args) (state_declare (parameter_current parameters) state))) ) )))

; Creates the layer necessary for a newly-created function call.
; This will set up lambdas for all jump functions that remove this block before the jump continues.
; Lambdas must be provided that will be run when the function exits through a return or thrown exception.
(define funcall_createNewLayer
  (lambda (return catch state)
    (state_update 'function 'error
    (state_update 'return   (lambda (state2) (return state2))                                             (state_declare 'return
    (state_update 'break    (lambda (state2) (error 'badbreak    "Oops, break lead out of function!"   )) (state_declare 'break
    (state_update 'continue (lambda (state2) (error 'badcontinue "Oops, continue lead out of function!")) (state_declare 'continue
    (state_update 'catch    (lambda (state2) (catch  state2))                                             (state_declare 'catch (pushlayer state) )) )) )) )) ) ))

; This is the "meat & potatoes" core of the function interpreter.
; Effectively, this does all of the steps necessary to execute the given function.
; This is effectively running down the following list of items:
; 1: Evaluate any state changes resulting from the argument expressions
; 2: Remove any state that the function can't see
; 3: Add a layer for the function to execute in
; 4: Evaluate the actual values of the argument expressions
; 5: Bind all of the argument expressions to their values
; 6: Execute the function itself
; 7: Remove the function's execution layer
; 8: Set the function return value correctly if a return occurred, or set it to an error value if no return occurred
; 9: Recombine the (potentially modified) state that the function could see with the state that it couldn't see
; (state_lookup 'catch state) (state_functioninvisible (funcall_name in) (funcall_evalargs_resultstate (funcall_args in) state))
(define funcall_resultstate
  (lambda (in state)
    ;; #9 ;;
    (state_combineinvisiblevisible
     (state_functioninvisible (funcall_name in) (funcall_evalargs_resultstate (funcall_args in) state))
     (call/cc
      (lambda (return)
        ;; #8 ;;
        (state_update
         'function
         'error
         ;; #7 ;;
         (poplayer
          ;; #6 ;;
          (statement_state
           (cons 'begin (function_body (state_lookup (funcall_name in) state)))
           ;; #5 ;;
           (funcall_bindargs2parameters_resultstate
            ;; #4 ;;
            (funcall_evalargs_valuelist (funcall_args in) state)
            (function_parameters (state_lookup (funcall_name in) state))
            ;; #3 ;;
            (funcall_createNewLayer
                                      ;; #8 through function return callback ;;
             (lambda (state2) (return (state_update 'function (state_lookup 'return state2) (poplayer state2))))
                                                                                                             ;; #9 through exception catch callback ;;
             (lambda (state2) ((state_lookup 'catch state) (state_update 'catch (state_lookup 'catch state2) (state_combineinvisiblevisible (state_functioninvisible (funcall_name in) (funcall_evalargs_resultstate (funcall_args in) state)) (poplayer state2)))))
             ;; #2 ;;
             (state_functionvisible
              (funcall_name in)
              ;; #1 ;;
              (funcall_evalargs_resultstate (funcall_args in) state))))))))))))

; Gets the result (return) value from the most recent function call in the given state.
; Test case: (funcall_resultvalue '(((function 0))))
;         -> 0
; Test case: (funcall_resultvalue '(((x 0))))
;         -> #error#
; Test case: (funcall_resultvalue '(((function error))))
;         -> #error#
(define funcall_resultvalue
  (lambda (state)
    (cond
      ((not (state_isdec 'function state))         (error 'badresult "Oops, tried to get function result without valid top-level result state!")                )
      ((eq? 'error (state_lookup 'function state)) (error 'badresult "Oops, tried to return an undeclared value or tried to assign from a no-return function!") )
      (else                                        (state_lookup 'function state)                                                                               ) )))

;----------------------------------------------------------------------;
; RETURN

; Get the 'expression' part of the given return statement
; Test case: (return_expression '(return (+ y 1)))
;         -> '(+ y 1)
(define return_expression cadr)

; Get the return value from a state.
; This state was (likely) the result state from the execution of some list of statements.
; If those statements contained a call to 'return', that return value was placed into the state.
; This function will return that return value, or throw an error if it is not found (e.g. there was no return call).
; Test case: (state_return '(((x 5) (return 10))))
;         -> 10
; Test case: (state_return '(((x 5) (z 10))))
;         -> #error#
(define state_return
  (lambda (state)
    (cond
      ((and (state_isdec 'return state) (not (procedure? (state_lookup 'return state)))) (state_lookup 'return state)                                )
      (else                                                                              (error 'noreturn "Oops, code completed without returning!") ) )))

; Return the state that results from executing the given return statement
; This state will have the 'return' variable declared and set to the return value.
; Test case: (return_resultstate '(return 2) (list (list (list 'return (lambda (v) v)))))
;         -> '(((return 2)))
; Test case: (return_resultstate '(return x) (list (list '(x 5) (list 'return (lambda (v) v)))))
;         -> '(((x 5) (return 5)))
; Test case: (return_resultstate '(return (= x 10)) (list (list '(x 5) (list 'return (lambda (v) v)))))
;         -> '(((x 10) (return 10)))
(define return_resultstate
  (lambda (in state)
    (cond
      ((and (state_isdec 'return state) (procedure? (state_lookup 'return state))) ((state_lookup 'return state) (state_update 'return (expression_value (return_expression in) state) (expression_state (return_expression in) state))) )
      (else                                                                        (error 'noreturn "Oops, attempted to return with no valid return function")                                                                           ) )))

;----------------------------------------------------------------------;
; IF

; Get the 'conditional' part of the given if statement
; Test case: (if_conditional '(if (> (+ x y) 30) (return x) (return y)))
;         -> '(> (+ x y) 30)
(define if_conditional cadr)

; Get the 'then statement' part of the given if statement
; Test case: (if_thenstatement '(if (> (+ x y) 30) (return x) (return y)))
;         -> '(return x)
(define if_thenstatement caddr)

; Get the 'else statement' part of the given if statement
; Test case: (if_elsestatement '(if (> (+ x y) 30) (return x) (return y)))
;         -> '(return y)
(define if_elsestatement cadddr)

; Return #t if the given if statement has an else statement, and #f otherwise
; Test case: (if_has_elsestatement '(if (> (+ x y) 30) (return x) (return y)))
;         -> #t
; Test case: (if_has_elsestatement '(if (> (+ x y) 30) (return x)))
;         -> #f
(define if_has_elsestatement
  (lambda (statement)
    (not (null? (cdddr statement)))))

; Return the state that results from executing the given if statement
; Test case: (if_resultstate '(if true (return x)) (list (list '(x 30) (list 'return (lambda (v) v)))))
;         -> '(((x 30) (return 30)))
; Test case: (if_resultstate '(if false (return x)) '(((x 30))))
;         -> '(((x 30)))
; Test case: (if_resultstate '(if (> (+ x y) 30) (return x) (return y)) (list (list '(x 1) '(y 2) (list 'return (lambda (v) v)))))
;         -> '(((x 1) (y 2) (return 2)))
; Test case: (if_resultstate '(if (> (+ x y) 30) (return x) (return y)) (list (list '(x 30) '(y 20) (list 'return (lambda (v) v)))))
;         -> '(((x 30) (y 20) (return 30)))
; Test case: (if_resultstate '(if (> (= x y) 30) (return 1) (return 2)) (list (list '(x 1) '(y 31) (list 'return (lambda (v) v)))))
;         -> '(((x 31) (y 31) (return 1)))
(define if_resultstate
  (lambda (in state)
    (cond
      (                               (eq? (expression_value (if_conditional in) state) 'true )  (statement_state (if_thenstatement in) (expression_state (if_conditional in) state)) )
      ((and (if_has_elsestatement in) (eq? (expression_value (if_conditional in) state) 'false)) (statement_state (if_elsestatement in) (expression_state (if_conditional in) state)) )
      (else                                                                                                                             (expression_state (if_conditional in) state)  ) )))

;----------------------------------------------------------------------;
; BREAK

; Returns the state that results from executing the given break statement
(define break_resultstate
  (lambda (in state)
    (cond
      ((not (state_isdec 'break state))               (error 'badbreak "Oops, attempted to break with no possible break function!") )
      ((not (procedure? (state_lookup 'break state))) (error 'badbreak "Oops, attempted to break with no valid break function!")    )
      (else                                           ((state_lookup 'break state) state)                                           ) )))

;----------------------------------------------------------------------;
; CONTINUE

; Returns the state that results from executing the given continue statement
(define continue_resultstate
  (lambda (in state)
    (cond
      ((not (state_isdec 'continue state))               (error 'badcontinue "Oops, attempted to continue with no possible continue function!") )
      ((not (procedure? (state_lookup 'continue state))) (error 'badcontinue "Oops, attempted to continue with no valid continue function!")    )
      (else                                              ((state_lookup 'continue state) state)                                                 ) )))

;----------------------------------------------------------------------;
; TRY

; Get the 'statement' part of the given try statement
; Test case: (try_statement '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x 2)))))
;         -> '((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5)))
(define try_statement cadr)

; Get the 'variable' part of the given try statement's catch block
; Test case: (try_catch_variable '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x 2)))))
;         -> 'e
(define try_catch_variable
  (lambda (statement)
    (caar (cdaddr statement))))

; Get the 'statement' part of the given try statement's catch block
; Test case: (try_catch_statement '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x 2)))))
;         -> '((= x e))
(define try_catch_statement
  (lambda (statement)
    (cadr (cdaddr statement))))

; Get the 'statement' part of the given try statement's finally block
; Test case: (try_finally_statement '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x 2)))))
;         -> '((= x 2))
(define try_finally_statement
  (lambda (statement)
    (cadr (cadddr statement))))

; Return #t if the given try statement has a finally block, and #f otherwise
; Test case: (try_has_finally '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x 2)))))
;         -> #t
; Test case: (try_has_finally '(try ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e)))))
;         -> #f
(define try_has_finally
  (lambda (statement)
    (not (null? (cadddr statement)))))

; Add a 'wrapper' around the jump calls inside a try block.
; This wrapper will ensure that the finally block is called before they complete.
; Note that 'return' needs a bit of special code here to ensure that a normal return from catch has its actual return value passed back appropriately.
; Test case: (try_wrapjumps '(try) 5 '(((break 1) (continue 2) (return 3) (catch 4) (x 8))))
;         -> '(((break ...) (continue ...) (return ...) (catch ...) (x 8)))
(define try_wrapjumps
  (lambda (in catch state_pre_try)
    (state_update 'catch    (lambda (state2) (catch (catch_resultstate in state_pre_try state2)))
    (state_update 'break    (lambda (state2) ((state_lookup 'break    state_pre_try)                                                     (finally_resultstate in (try_unwrapjumps state_pre_try state2)) ))
    (state_update 'continue (lambda (state2) ((state_lookup 'continue state_pre_try)                                                     (finally_resultstate in (try_unwrapjumps state_pre_try state2)) ))
    (state_update 'return   (lambda (state2) ((state_lookup 'return   state_pre_try) (state_update 'return (state_lookup 'return state2) (finally_resultstate in (try_unwrapjumps state_pre_try state2))))) state_pre_try))))))

; Remove the wrapper added around jump calls by 'try_wrapjumps'
; Otherwise, the wrapper could persist after the try block completes, causing the finally block to be run by proceeding statements.
; Test case: (try_unwrapjumps '(((break 1) (continue 2) (return 3) (catch 4) (x 53))) (try_wrapjumps '(try) 5 '(((break 1) (continue 2) (return 3) (catch 4) (x 11)))))
;         -> '(((break 1) (continue 2) (return 3) (catch 4) (x 11)))
(define try_unwrapjumps
  (lambda (state_pre_try state_to_unwrap)
    (state_update 'catch    (state_lookup 'catch    state_pre_try)
    (state_update 'break    (state_lookup 'break    state_pre_try)
    (state_update 'continue (state_lookup 'continue state_pre_try)
    (state_update 'return   (state_lookup 'return   state_pre_try) state_to_unwrap))))))

; Returns the state that results from executing the given try statement
(define try_resultstate
  (lambda (in state)

    ; Always run finally block after everything
    (finally_resultstate in
    
    ; Restore previous catch/break/continue/return function once we're done
    (try_unwrapjumps state

    ; Return point for thrown exceptions
    (call/cc
     (lambda (catch)
       (statement_state (cons 'begin (try_statement in)) (try_wrapjumps in catch state))))))))

; Returns the state that results from executing the given catch statement
; This requires access to both 'state1', the state before the try statement ran,
; and 'state2', the state after it ran.
; This is because of re-throwing. We have to reset the 'catch' variable to its state
; before the try statement ran before we can run the catch statement.
; Otherwise, the thrown value that triggered this catch statement will be stuck in place of the exception target.
(define catch_resultstate
  (lambda (in state1 state2)
    (statement_state (cons 'begin (try_catch_statement in)) (state_update (try_catch_variable in) (state_lookup 'catch state2) (state_declare (try_catch_variable in) (state_update 'catch (state_lookup 'catch state1) state2))))))

; Returns the state that results from executing the given finally statement
; Note that not all try/catch blocks have finally statements
(define finally_resultstate
  (lambda (in state)
    (if (try_has_finally in)
        (statement_state (cons 'begin (try_finally_statement in)) state)
        state)))

;----------------------------------------------------------------------;
; THROW

; Get the 'expression' part of the given throw statement
; Test case: (throw_expression '(throw (+ y 1)))
;         -> '(+ y 1)
(define throw_expression cadr)

; Returns the state that results from executing the given throw statement
(define throw_resultstate
  (lambda (in state)
    (cond
      ((not (state_isdec 'catch state))               (error 'badthrow "Oops, attempted to throw with no possible catch function!")                                                                     )
      ((not (procedure? (state_lookup 'catch state))) (error 'badthrow "Oops, attempted to throw with no valid catch function!")                                                                        )
      (else                                           ((state_lookup 'catch state) (state_update 'catch (expression_value (throw_expression in) state) (expression_state (throw_expression in) state))) ) )))

;----------------------------------------------------------------------;
; WHILE

; Get the 'conditional' part of the given while statement
; Test case: (while_conditional '(while true (= y (+ y 1))))
;         -> 'true
(define while_conditional cadr)

; Get the 'body statement' part of the given while statement
; Test case: (while_bodystatement '(while true (= y (+ y 1))))
;         -> '(= y (+ y 1))
(define while_bodystatement caddr)

; Return the state that results from executing the given while statement
; break and continue continuations go here
; Test case: (while_resultstate '(while (< x 10) (= x (+ x 1))) '(((x 0) (break error) (continue error))))
;         -> '(((x 10) (break error) (continue error)))
(define while_resultstate
  (lambda (in state)

    ; Restore previous break/continue functions once we're done
    (state_update 'break    (state_lookup 'break    state)
    (state_update 'continue (state_lookup 'continue state)

    ; Return point for break calls
    (call/cc
     (lambda (break)
       (cond
         ((eq? (expression_value (while_conditional in) (state_update 'break break state)) 'true ) (while_resultstate in (call/cc
                                                                                                                         (lambda (continue)
                                                                                                                           (statement_state (while_bodystatement in) (expression_state (while_conditional in) (state_update 'break break (state_update 'continue continue state))))))) )
         (else                                                                                                                                                       (expression_state (while_conditional in) (state_update 'break break                                  state ))     ) )))))))

;----------------------------------------------------------------------;
; BLOCK

; Return true if the current statement is the final line in the block
; Test case: (body_end? '((var y 2) (= x y)))
;         -> '#f
; Test case: (body_end? '((var y 2)))
;         -> '#t
(define body_end?
  (lambda (in)
    (cond
      ((null? (cdr in)) #t)
      (else             #f))))

; Return the first line in a block
; Test case: (block_first '((var y 2) (var z (* x y)) (= x z)))
;         -> '(var y 2)
(define block_first car)

; Return the remaining lines in a block
; Test case: (block_remaining '((var y 2) (var z (* x y)) (= x z)))
;         -> '((var z (* x y)) (= x z))
(define block_remaining cdr)

; Return the state that results from executin a block of code
; Test case: (block_resultstate '((return 150)) '(() ()))
;         -> '((return 150))
; Test case: (block_resultstate '((var y 2) (= x y)) '(() ((x 10))))
;         -> '((x 2))
; Test case: (block_resultstate '((var y 2) (var z (* x y)) (= x z)) '(() ((x 10))))
;         -> '((x 20))
; Test case: (block_resultstate '() '(((y 2) (return x)) ((x 10))))
;         -> '((x 10) (return x))
(define block_resultstate
  (lambda (in state)
    (cond
      ((null? in)           (poplayer state)                                                                 )
      ((not (body_end? in)) (block_resultstate (block_remaining in) (statement_state (block_first in) state)))
      (else                 (block_resultstate '()                  (statement_state (block_first in) state))))))

; Returns the state used to start the interpreter, given a global return function.
; This sets up default values for all jump functions.
(define block_initialState
  (lambda ()
    (state_update 'function 'error                                                                      (state_declare 'function
    (state_update 'return   (lambda (state) (error 'badbreak    "Oops, return lead out of program!"  )) (state_declare 'return
    (state_update 'break    (lambda (state) (error 'badbreak    "Oops, break lead out of program!"   )) (state_declare 'break
    (state_update 'continue (lambda (state) (error 'badcontinue "Oops, continue lead out of program!")) (state_declare 'continue
    (state_update 'catch    (lambda (state) (error 'badthrow    "Oops, exception went unhandled!"    )) (state_declare 'catch '(()) )) )) )) )) )) ))

; Creates the layer necessary for a newly-created block.
; This will set up lambdas for all jump functions that remove this block before the jump continues.
; A bit of special code is also needed for return & catch, as they must propagate their return value down
; through the "return"/"catch" variable as all of the layers are being popped off.
(define block_createNewLayer
  (lambda (state)
    (state_update 'return   (lambda (state2) ((state_lookup 'return   state) (state_update 'return (state_lookup 'return state2) (poplayer state2)))) (state_declare 'return
    (state_update 'break    (lambda (state2) ((state_lookup 'break    state)                                                     (poplayer state2) )) (state_declare 'break
    (state_update 'continue (lambda (state2) ((state_lookup 'continue state)                                                     (poplayer state2) )) (state_declare 'continue
    (state_update 'catch    (lambda (state2) ((state_lookup 'catch    state) (state_update 'catch  (state_lookup 'catch  state2) (poplayer state2)))) (state_declare 'catch (pushlayer state) )) )) )) )) ))

;----------------------------------------------------------------------;
; CORE

; Get the state that results from the execution of the given single statement
; Note that this also does execute blocks (begin ...), which are multiple statements glommed together into a single "statement".
(define statement_state
  (lambda (in state)
    (cond
      ((null? in) (error 'parse "Oops, no statement given!"))
      ((eq? 'var      (statement_operator in)) (declaration_resultstate in       state                        ))
      ((eq? '=        (statement_operator in)) (assignment_resultstate  in       state                        ))
      ((eq? 'begin    (statement_operator in)) (block_resultstate       (cdr in) (block_createNewLayer state) ))
      ((eq? 'return   (statement_operator in)) (return_resultstate      in       state                        ))
      ((eq? 'if       (statement_operator in)) (if_resultstate          in       state                        ))
      ((eq? 'break    (statement_operator in)) (break_resultstate       in       state                        ))
      ((eq? 'continue (statement_operator in)) (continue_resultstate    in       state                        ))
      ((eq? 'while    (statement_operator in)) (while_resultstate       in       state                        ))
      ((eq? 'try      (statement_operator in)) (try_resultstate         in       state                        ))
      ((eq? 'throw    (statement_operator in)) (throw_resultstate       in       state                        ))
      ((eq? 'function (statement_operator in)) (function_resultstate    in       state                        ))
      ((eq? 'funcall  (statement_operator in)) (funcall_resultstate     in       state                        ))
      (else (error 'badop "Oops, bad statement given!")))))

; Wrapper that calls the interpreter on the given file
; This is done in a few steps, working from inside to outside
;  - 'parser' Converts the file into a syntax tree.
;  - 'block_initialState' Creates initial targets for jumps (e.g. return, break, etc).
;  - 'funcall' Adds a call at the end of the program that calls the main method itself.
;  - 'statement_state' Produces the state resulting from the execution of the syntax tree.
;  - 'funcall_resultvalue' Pulls the return value out of the syntax tree in the event of a normal return.
; Note that we're nice and generous and allow _any_ statement at the top level. Cool!
(define interpret
  (lambda (file)
    (funcall_resultvalue (statement_state (cons 'begin (append (parser file) '((funcall main)))) (block_initialState) ))))