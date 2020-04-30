#lang racket
(provide (all-defined-out))
(require "Interpreter4State.rkt")
(require "Interpreter4Heap.rkt")

;----------------------------------------------------------------------;

; By Kienan Ahner-McHaffie (kta12)
; and Anna Dutkiewicz (amd219)

;----------------------------------------------------------------------;
; CLASS

; PUBLIC

; Sets up the class handler for the given state
(define state_setupclass
  (lambda (state)
    (state_update 'super '() (state_update 'class nullptr (state_declare 'super (state_declare 'class state))))))

; Loads all of the classes parsed into the given atom into a state
(define state_loadparsedclasses
  (lambda (parsedclasses state)
    (state_setclasslist (append (parsedclasses_createclasslist parsedclasses state) (state_getclasslist state)) (parsedclasses_createclasslist_resultstate parsedclasses state)) ))

; Returns the classname atom of the currently-executing class
(define class_getcurrent
  (lambda (state)
    (state_lookup 'class state)))

; Updates the classname atom of the currently-executing class
(define class_setcurrent
  (lambda (class state)
    (state_update 'class class state)))

; Checks whether a class with the given name exists
(define class_exists
  (lambda (class state)
    (classlist_hasentry class (state_getclasslist state))))

; Gets the static object pointer for the given class
(define class_getstaticptr
  (lambda (class state)
    (classlistentry_getstaticptr (classlist_getentry class (state_getclasslist state))) ))

; Checks if the given class has a function with the given name
(define class_hasfunction
  (lambda (class name state)
    (funclist_hasentry name (classlistentry_getfunclist (classlist_getentry class (state_getclasslist state)))) ))

; Returns the function with the given name for the given class
(define class_getfunction
  (lambda (class name state)
    (funclist_getentry name (classlistentry_getfunclist (classlist_getentry class (state_getclasslist state)))) ))

; Returns a list of the names of the non-static variables associated with the given class.
(define class_getvarlist
  (lambda (class state)
    (classlistentry_getvarlist (classlist_getentry class (state_getclasslist state))) ))

; PRIVATE

; Gets the state's classlist
(define state_getclasslist
  (lambda (state)
    (state_lookup 'super state)))

; Updates the state's classlist
(define state_setclasslist
  (lambda (classlist state)
    (state_update 'super classlist state)))

; Creates a classlist based on the given set of parsed classes
(define parsedclasses_createclasslist
  (lambda (parsedclasses state)
    (cond
      ((null? parsedclasses)             '()                                                                                                                                                                                    )
      ((eq? 'class (caar parsedclasses)) (cons (parsedclass_createclasslist (car parsedclasses) state) (parsedclasses_createclasslist (cdr parsedclasses) (parsedclass_createclasslist_resultstate (car parsedclasses) state))) )
      (else                              (error 'nonclass "Oops, non-class entry found at top level!")                                                                                                                          ) )))

(define parsedclasses_createclasslist_resultstate
  (lambda (parsedclasses state)
    (cond
      ((null? parsedclasses)             state                                                                                                                               )
      ((eq? 'class (caar parsedclasses)) (parsedclass_createclasslist_resultstate (car parsedclasses) (parsedclasses_createclasslist_resultstate (cdr parsedclasses) state)) )
      (else                              (error 'nonclass "Oops, non-class entry found at top level!")                                                                       ) )))

(define parsedclass_createclasslist
  (lambda (parsedclass state)
    (list (parsedclass_createclassname         parsedclass )
          (parsedclass_createvarlist   (cadddr parsedclass))
          (heap_new_resultptr state                        )
          (parsedclass_createfunclist  (cadddr parsedclass)) ) ))

(define parsedclass_createclasslist_resultstate
  (lambda (parsedclass state)
    (heap_new_resultstate (cons (parsedclass_createclassname parsedclass) (parsedclass_createstaticobj (cadddr parsedclass))) state) ))

(define parsedclass_createclassname
  (lambda (parsedclass)
    (cadr parsedclass)))

(define parsedclass_createvarlist
  (lambda (parsedclass)
    (cond
      ((null? parsedclass)                                                                       '()                                                                                                  )
      ((and (list? (car parsedclass)) (eq? 'var (caar parsedclass)) (null? (cddar parsedclass))) (cons (list (cadar parsedclass)                    0) (parsedclass_createvarlist (cdr parsedclass))) )
      ((and (list? (car parsedclass)) (eq? 'var (caar parsedclass))                            ) (cons (list (cadar parsedclass) (caddar parsedclass)) (parsedclass_createvarlist (cdr parsedclass))) )
      (else                                                                                      (parsedclass_createvarlist (cdr parsedclass))                                                        ) )))

(define parsedclass_createstaticobj
  (lambda (parsedclass)
    (cond
      ((null? parsedclass)                                                  '()                                                                                 )
      ((and (list? (car parsedclass)) (eq? 'static-var (caar parsedclass))) (cons (list (cadar parsedclass) 0) (parsedclass_createstaticobj (cdr parsedclass))) )
      (else                                                                 (parsedclass_createstaticobj (cdr parsedclass))                                     ) )))

(define parsedclass_createfunclist
  (lambda (parsedclass)
    (cond
      ((null? parsedclass)                                                       '()                                                                     )
      ((and (list? (car parsedclass)) (eq? 'function        (caar parsedclass))) (cons (car parsedclass) (parsedclass_createfunclist (cdr parsedclass))) )
      ((and (list? (car parsedclass)) (eq? 'static-function (caar parsedclass))) (cons (car parsedclass) (parsedclass_createfunclist (cdr parsedclass))) )
      (else                                                                      (parsedclass_createfunclist (cdr parsedclass))                          ) )))

; Check whether a class exists in the classlist with the given name
(define classlist_hasentry
  (lambda (class classlist)
    (cond
      ((null? classlist)                                         #f                                         )
      ((eq? class (classlistentry_getclassname (car classlist))) #t                                         )
      (else                                                      (classlist_hasentry class (cdr classlist)) ) )))

; Find a class in the classlist with the given name
(define classlist_getentry
  (lambda (class classlist)
    (cond
      ((null? classlist)                                         (error 'badclass "Oops, requested class not found!") )
      ((eq? class (classlistentry_getclassname (car classlist))) (car classlist)                                      )
      (else                                                      (classlist_getentry class (cdr classlist))           ) )))

; Gets the class name from a classlistentry
(define classlistentry_getclassname
  (lambda (classlistentry)
    (car classlistentry)))
     
; Gets the non-static variable list from a classlistentry
(define classlistentry_getvarlist
  (lambda (classlistentry)
    (cadr classlistentry)))

; Gets the static object pointer from a classlistentry
(define classlistentry_getstaticptr
  (lambda (classlistentry)
    (caddr classlistentry)))

; Gets the function list from a classlistentry
(define classlistentry_getfunclist
  (lambda (classlistentry)
    (cadddr classlistentry)))

; Gets the function name from a funclistentry
(define funclistentry_getfuncname
  (lambda (funclistentry)
    (cadr funclistentry)))

; Check whether the given function list contains a function with the given name
(define funclist_hasentry
  (lambda (func funclist)
    (cond
      ((null? funclist)                                      #f                                      )
      ((eq? func (funclistentry_getfuncname (car funclist))) #t                                      )
      (else                                                  (funclist_hasentry func (cdr funclist)) ) )))

; Find a function in the function list with the given name
(define funclist_getentry
  (lambda (func funclist)
    (cond
      ((null? funclist)                                      (error 'badfunc "Oops, requested function not found!") )
      ((eq? func (funclistentry_getfuncname (car funclist))) (car funclist)                                         )
      (else                                                  (funclist_getentry func (cdr funclist))                ) )))

;----------------------------------------------------------------------;
; INSTANCE

(define state_setupinstance
  (lambda (state)
    (state_update 'this nullptr (state_declare 'this state))))
; instance_getthis state
(define instance_getthis
  (lambda (state)
    (cond
      ((state_isdec 'this state) (state_lookup 'this state) )
      (else                      nullptr                    ) )))
; instance_setthis ptr state
(define instance_setthis
  (lambda (ptr state)
    (state_update 'this ptr state)))
; instance_getvar name ptr state
(define instance_getvar
  (lambda (name ptr state)
    (instance_getvar_impl name (cdr (ptr_dereference ptr state)))))
; 
(define instance_getvar_impl
  (lambda (name varlist)
    (cond
      ((null? varlist)           (error 'badlookup "Oops, couldn't find child variable!") )
      ((eq? name (caar varlist)) (cadar varlist)                                          )
      (else                      (instance_getvar_impl name (cdr varlist))                ) )))
; instance_setvar name ptr state
(define instance_setvar
  (lambda (name value ptr state)
    (ptr_setvalue ptr (cons (car (ptr_dereference ptr state)) (instance_setvar_impl name value (cdr (ptr_dereference ptr state)))) state) ))
; 
(define instance_setvar_impl
  (lambda (name value varlist)
    (cond
      ((null? varlist)           (error 'badset "Oops, couldn't find child variable to set!")         )
      ((eq? name (caar varlist)) (cons (list name value) (cdr varlist))                               )
      (else                      (cons (car varlist) (instance_setvar_impl name value (cdr varlist))) ) )))
; instance_getsuper ptr
(define instance_getsuper
  (lambda (ptr)
    (error 'madeit "Nice!")))
; instance_create_resultptr args state
(define instance_create_resultptr
  (lambda (args state)
    (heap_new_resultptr state)))
; instance_create_resultstate args state
(define instance_create_resultstate
  (lambda (args state)
    (heap_new_resultstate (cons (car args) (class_getvarlist (car args) state)) state)))
; instance_isthisdec name state
(define instance_isthisdec
  (lambda (name state)
    (and (not (ptr_isnull (instance_getthis state))) (instance_hasvar name (instance_getthis state) state))))
; instance_hasvar name ptr state
(define instance_hasvar
  (lambda (name ptr state)
    (instance_hasvar_impl name (cdr (ptr_dereference ptr state)))))
; instance_hasvar_impl name varlist
(define instance_hasvar_impl
  (lambda (name varlist)
    (cond
      ((null? varlist)           #f                                        )
      ((eq? name (caar varlist)) #t                                        )
      (else                      (instance_hasvar_impl name (cdr varlist)) ) )))

; Returns the classname atom of the given instance
(define instance_getclass
  (lambda (ptr state)
    (car (ptr_dereference ptr state))))