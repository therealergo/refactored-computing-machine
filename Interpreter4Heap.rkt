#lang racket
(provide (all-defined-out))
(require "Interpreter4State.rkt")

;----------------------------------------------------------------------;

; By Kienan Ahner-McHaffie (kta12)
; and Anna Dutkiewicz (amd219)

;----------------------------------------------------------------------;
; HEAP
; A beautiful, fast heap implementation. No cheaty non-functional-ness here!
; It takes a state-of-the art patent-pending new approach to memory management:
; Memory, once allocated, is never and can never be freed.
; (some may recognize this as the memory management approach used by Google Chrome)
; Features all of the lightning-fast features a modern programmer needs:
;  - Dereference pointers in subexponential time!
;  - Allocate memory in O(1) time (for variable values of 1)!
;  - Floating point heap pointers are supported and encouraged!
;  - A free *BONUS* ultra-fast pointer to the number seven (if you can find it)!
;  - Error checking for several possible error states!

; Returns the heap from the given state
(define state_getheap
  (lambda (state)
    (state_lookup 'new state)))

; Returns a state with its heap updated to reflect the supplied heap.
(define state_updateheap
  (lambda (heap state)
    (state_update 'new heap state)))

; Creates a new heap in the given state at the current layer.
(define state_createheap
  (lambda (state)
    (state_update 'new '(2 7) (state_declare 'new state))))

; Returns the size of the given heap.
(define heap_getsize
  (lambda (heap)
    (car heap)))

; Get the state that results from adding the given object to the given state's heap.
; Test case: (heap_new_resultstate 'a (state_createheap '(())))
;         -> '((('new (2 a))))   ;)
; Test case: (heap_new_resultstate 'b (heap_new_resultstate 'a (state_createheap '(()))))
;         -> '((('new (3 a b))))   ;)
(define heap_new_resultstate
  (lambda (obj state)
    (state_updateheap (heap_new_resultheap obj (state_getheap state)) state)))

; Impl for heap_new_resultstate.
(define heap_new_resultheap
  (lambda (obj heap)
    (cons (+ (heap_getsize heap) 1) (append (cdr heap) (list obj)))))

; Get the pointer to the object created on the given heap with the next heap_new_resultheap call.
; Test case: (heap_new_resultptr (state_createheap '(())))
;         -> 2
; Test case: (heap_new_resultptr (heap_new_resultstate 'a (state_createheap '(()))))
;         -> 3
(define heap_new_resultptr
  (lambda (state)
    (heap_getsize (state_getheap state))))

; Dereference a heap pointer.
; Test case: (ptr_dereference 2 (heap_new_resultstate 'a (state_createheap '(()))))
;         -> 'a
; Test case: (ptr_dereference 2 (heap_new_resultstate 'b (heap_new_resultstate 'a (state_createheap '(())))))
;         -> 'a
; Test case: (ptr_dereference 3 (heap_new_resultstate 'b (heap_new_resultstate 'a (state_createheap '(())))))
;         -> 'b
(define ptr_dereference
  (lambda (ptr state)
    (cond
      ((not (number? ptr)) (error 'badptr "Oops, Tried to dereference something that wasn't a pointer!") )
      ((eq? ptr nullptr)   (error 'badptr "Oops, Tried to dereference null pointer!")                    )
      (else                (ptr_dereferencefromheap ptr (state_getheap state))                           ) )))

; Impl for ptr_dereference.
; WARN: No error checking!
(define ptr_dereferencefromheap
  (lambda (ptr heap)
    (cond
      ((null? heap) (error 'badptr "Oops, Tried to dereference pointer that was outside of heap!") )
      ((eq? ptr 0)  (car heap)                                                                     )
      (else         (ptr_dereferencefromheap (- ptr 1) (cdr heap))                                 ) )))

; Returns the state that results from setting the value pointed to by the given pointer
; Test case: (ptr_setvalue 2 'c (heap_new_resultstate 'a (state_createheap '(()))))
;         -> '(((new (2 c))))   ;)
; Test case: (ptr_setvalue 2 'c (heap_new_resultstate 'b (heap_new_resultstate 'a (state_createheap '(())))))
;         -> '(((new (3 c b))))   ;)
; Test case: (ptr_setvalue 3 'c (heap_new_resultstate 'b (heap_new_resultstate 'a (state_createheap '(())))))
;         -> '(((new (3 a c))))   ;)
(define ptr_setvalue
  (lambda (ptr value state)
    (cond
      ((not (number? ptr)) (error 'badptr "Oops, Tried to set value of something that wasn't a pointer!") )
      ((eq? ptr nullptr)   (error 'badptr "Oops, Tried to set value of null pointer!")                    )
      (else                (state_updateheap (ptr_setvalueinheap ptr value (state_getheap state)) state)  ) )))

; Impl for ptr_setvalue.
; WARN: No error checking!
(define ptr_setvalueinheap
  (lambda (ptr value heap)
    (cond
      ((null? heap) (error 'badptr "Oops, Tried to set value of pointer that was outside of heap!") )
      ((eq? ptr 0)  (cons value (cdr heap))                                                         )
      (else         (cons (car heap) (ptr_setvalueinheap (- ptr 1) value (cdr heap)))               ) )))

; The null pointer, which is 0.
(define nullptr 0)

; Returns true if the given pointer is null, false otherwise
(define ptr_isnull
  (lambda (ptr)
    (eq? ptr 0)))