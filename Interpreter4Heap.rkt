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
    (state_lookup_impl 'super state)))

; Returns a state with its heap updated to reflect the supplied heap.
(define state_updateheap
  (lambda (state heap)
    (state_update_impl 'super heap state)))

; Creates a new heap in the given state at the current layer.
(define state_createheap
  (lambda (state)
    (state_update_impl 'super '(2 7) (state_declare_impl 'super state))))

; Returns the size of the given heap.
(define heap_getsize
  (lambda (heap)
    (car heap)))

; Get the state that results from adding the given object to the given state's heap.
; Test case: (heap_add_resultstate (state_createheap '(())) 'a)
;         -> '((('super (2 a))))
; Test case: (heap_add_resultstate (heap_add_resultstate (state_createheap '(())) 'a) 'b)
;         -> '((('super (3 a b))))
(define heap_add_resultstate
  (lambda (state obj)
    (state_updateheap state (heap_add_resultheap (state_getheap state) obj))))

; Impl for heap_add_resultstate.
(define heap_add_resultheap
  (lambda (heap obj)
    (cons (+ (heap_getsize heap) 1) (append (cdr heap) (list obj)))))

; Get the pointer to the object created on the given heap with the next heap_add_resultheap call.
; Test case: (heap_add_resultptr (state_createheap '(())))
;         -> 2
; Test case: (heap_add_resultptr (heap_add_resultstate (state_createheap '(())) 'a))
;         -> 3
(define heap_add_resultptr
  (lambda (state)
    (heap_getsize (state_getheap state))))

; Dereference a heap pointer.
; Test case: (ptr_dereference (heap_add_resultstate (state_createheap '(())) 'a) 2)
;         -> 'a
; Test case: (ptr_dereference (heap_add_resultstate (heap_add_resultstate (state_createheap '(())) 'a) 'b) 2)
;         -> 'a
; Test case: (ptr_dereference (heap_add_resultstate (heap_add_resultstate (state_createheap '(())) 'a) 'b) 3)
;         -> 'b
(define ptr_dereference
  (lambda (state ptr)
    (cond
      ((not (number? ptr)) (error 'badptr "Tried to dereference something that wasn't a pointer!") )
      ((eq? ptr 0)         (error 'badptr "Tried to dereference null pointer!")                    )
      (else                (ptr_dereferencefromheap (state_getheap state) ptr)                     ) )))

; Impl for ptr_dereference.
; WARN: No error checking!
(define ptr_dereferencefromheap
  (lambda (heap ptr)
    (cond
      ((null? heap) (error 'badptr "Tried to dereference pointer that was outside of heap!") )
      ((eq? ptr 0)  (car heap)                                                               )
      (else         (ptr_dereferencefromheap (cdr heap) (- ptr 1))                           ) )))