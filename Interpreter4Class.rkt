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

; Loads all of the classes stored in the given atom into a state
(define state_loadparsedclasses
  (lambda (parsedclasses state)
    (cond
      ((null? parsedclasses)             state                                                                                                                                                                                                                            )
      ((eq? 'class (caar parsedclasses)) (state_loadparsedclasses (cdr parsedclasses) (state_setclasslist (cons (parsedclass_createclasslist (car parsedclasses) state) (state_getclasslist state)) (parsedclass_createclasslist_resultstate (car parsedclasses) state))) ) )))

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
    (cond
      ((funclist_hasentry name (classlistentry_getfunclist (classlist_getentry class (state_getclasslist state)))) #t                                                          )
      ((class_hassuper class state)                                                                                (class_hasfunction (class_getsuper class state) name state) )
      (else                                                                                                        #f                                                          ) )))

; Returns the function with the given name for the given class
(define class_getfunction
  (lambda (class name state)
    (cond
      ((funclist_hasentry name (classlistentry_getfunclist (classlist_getentry class (state_getclasslist state)))) (funclist_getentry name (classlistentry_getfunclist (classlist_getentry class (state_getclasslist state)))) )
      ((class_hassuper class state)                                                                                (class_getfunction (class_getsuper class state) name state)                                                 )
      (else                                                                                                        (error 'nofunc "Oops, requested function not found!")                                                       ) )))

; Returns a list of the names of the non-static variables associated with the given class.
; Variables are organized into pairs with the name as the first member and the initialization value as the second member.
(define class_getvarlist
  (lambda (class state)
    (classlistentry_getvarlist (classlist_getentry class (state_getclasslist state))) ))

; Returns #t if the given-named class has a superclass, and #f otherwise
(define class_hassuper
  (lambda (class state)
    (not (eq? nullptr (classlistentry_getsuperptr (classlist_getentry class (state_getclasslist state)))))))

; Returns the name of the given-named class's superclass
(define class_getsuper
  (lambda (class state)
    (car (ptr_dereference (classlistentry_getsuperptr (classlist_getentry class (state_getclasslist state))) state))))

; PRIVATE

; Gets the state's classlist
(define state_getclasslist
  (lambda (state)
    (state_lookup 'super state)))

; Updates the state's classlist
(define state_setclasslist
  (lambda (classlist state)
    (state_update 'super classlist state)))

(define parsedclass_createclasslist
  (lambda (parsedclass state)
    (list (parsedclass_createclassname         parsedclass      )
          (heap_new_resultptr state                             )
          (parsedclass_createsuperptr          parsedclass state)
          (parsedclass_createvarlist   (cadddr parsedclass)     )
          (parsedclass_createfunclist  (cadddr parsedclass)     ) ) ))

(define parsedclass_createclasslist_resultstate
  (lambda (parsedclass state)
    (heap_new_resultstate (cons (parsedclass_createclassname parsedclass) (cons (parsedclass_createsuperptr parsedclass state) (parsedclass_createstaticobj (cadddr parsedclass)))) state) ))

(define parsedclass_createclassname
  (lambda (parsedclass)
    (cadr parsedclass)))

(define parsedclass_createstaticobj
  (lambda (parsedclass)
    (cond
      ((null? parsedclass)                                                                              '()                                                                                                    )
      ((and (list? (car parsedclass)) (eq? 'static-var (caar parsedclass)) (null? (cddar parsedclass))) (cons (list (cadar parsedclass)                    0) (parsedclass_createstaticobj (cdr parsedclass))) )
      ((and (list? (car parsedclass)) (eq? 'static-var (caar parsedclass))                            ) (cons (list (cadar parsedclass) (caddar parsedclass)) (parsedclass_createstaticobj (cdr parsedclass))) )
      (else                                                                                             (parsedclass_createstaticobj (cdr parsedclass))                                                        ) )))

(define parsedclass_createsuperptr
  (lambda (parsedclass state)
    (cond
      ((null? (caddr parsedclass)) nullptr                                          )
      (else                        (class_getstaticptr (car (cdaddr parsedclass)) state) ) )))

(define parsedclass_createvarlist
  (lambda (parsedclass)
    (cond
      ((null? parsedclass)                                                                       '()                                                                                                  )
      ((and (list? (car parsedclass)) (eq? 'var (caar parsedclass)) (null? (cddar parsedclass))) (cons (list (cadar parsedclass)                    0) (parsedclass_createvarlist (cdr parsedclass))) )
      ((and (list? (car parsedclass)) (eq? 'var (caar parsedclass))                            ) (cons (list (cadar parsedclass) (caddar parsedclass)) (parsedclass_createvarlist (cdr parsedclass))) )
      (else                                                                                      (parsedclass_createvarlist (cdr parsedclass))                                                        ) )))

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

; Gets the static object pointer from a classlistentry
(define classlistentry_getstaticptr
  (lambda (classlistentry)
    (cadr classlistentry)))

; Gets the superclass instance pointer from a classlistentry
(define classlistentry_getsuperptr
  (lambda (classlistentry)
    (caddr classlistentry)))
     
; Gets the non-static variable list from a classlistentry
(define classlistentry_getvarlist
  (lambda (classlistentry)
    (cadddr classlistentry)))

; Gets the function list from a classlistentry
(define classlistentry_getfunclist
  (lambda (classlistentry)
    (car (cddddr classlistentry))))

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
    (cond
      ((instance_hasvar_impl name (cddr (ptr_dereference ptr state)))                                                      (instance_getvar_impl name (cddr (ptr_dereference ptr state)))                                    )
      ((instance_hasvar_impl name (cddr (ptr_dereference (class_getstaticptr (instance_getclass ptr state) state) state))) (instance_getvar name (class_getstaticptr (instance_getclass ptr state) state) state) )
      ((not (eq? (instance_getsuperinstance ptr state) nullptr))                                                           (instance_getvar name (instance_getsuperinstance ptr state) state)                                )
      (else                                                                                                                (error 'badlookup "Oops, couldn't find child variable!")                                          ) )))
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
    (cond
      ((instance_hasvar_impl name (cddr (ptr_dereference ptr state)))                                                      (ptr_setvalue ptr (cons (car (ptr_dereference ptr state)) (cons (cadr (ptr_dereference ptr state)) (instance_setvar_impl name value (cddr (ptr_dereference ptr state))))) state) )
      ((instance_hasvar_impl name (cddr (ptr_dereference (class_getstaticptr (instance_getclass ptr state) state) state))) (instance_setvar name value (class_getstaticptr (instance_getclass ptr state) state) state)                                                                          )
      ((not (eq? (instance_getsuperinstance ptr state) nullptr))                                                           (instance_setvar name value (instance_getsuperinstance ptr state) state)                                                                                                         )
      (else                                                                                                                (error 'badset "Oops, couldn't find child variable to set!")                                                                                                                     ) )))
; 
(define instance_setvar_impl
  (lambda (name value varlist)
    (cond
      ((null? varlist)           (error 'badset "Oops, couldn't find child variable to set!")         )
      ((eq? name (caar varlist)) (cons (list name value) (cdr varlist))                               )
      (else                      (cons (car varlist) (instance_setvar_impl name value (cdr varlist))) ) )))
; instance_getsuper ptr
(define instance_getsuperinstance
  (lambda (ptr state)
    (cadr (ptr_dereference ptr state))))
; instance_create_resultptr args state
(define instance_create_resultptr
  (lambda (state)
    (heap_new_resultptr state)))
; instance_create_resultstate args state
(define instance_create_resultstate
  (lambda (args state)
    (cond
      ((class_hassuper (car args) state) (instance_create_resultstate (list (class_getsuper (car args) state)) (heap_new_resultstate (cons (car args) (cons (instance_create_resultptr (heap_new_resultstate '() state)) (class_getvarlist (car args) state))) state)) )
      (else                                                                                                    (heap_new_resultstate (cons (car args) (cons nullptr                                                      (class_getvarlist (car args) state))) state)  ) )))
; instance_isthisdec name state
(define instance_isthisdec
  (lambda (name state)
    (and (not (ptr_isnull (instance_getthis state))) (instance_hasvar name (instance_getthis state) state))))
; instance_hasvar name ptr state
(define instance_hasvar
  (lambda (name ptr state)
    (cond
      ((instance_hasvar_impl name (cddr (ptr_dereference ptr state)))                                                      #t                                                                 )
      ((instance_hasvar_impl name (cddr (ptr_dereference (class_getstaticptr (instance_getclass ptr state) state) state))) #t                                                                 )
      ((not (eq? (instance_getsuperinstance ptr state) nullptr))                                                           (instance_hasvar name (instance_getsuperinstance ptr state) state) )
      (else                                                                                                                #f                                                                 ) )))
; instance_hasvar_impl name varlist
(define instance_hasvar_impl
  (lambda (name varlist)
    (cond
      ((null? varlist)           #f                                        )
      ((eq? name (caar varlist)) #t                                        )
      (else                      (instance_hasvar_impl name (cdr varlist)) ) )))

; Returns the classname atom for the given instance
(define instance_getclass
  (lambda (ptr state)
    (car (ptr_dereference ptr state))))