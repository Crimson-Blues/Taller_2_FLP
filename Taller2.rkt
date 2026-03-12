#lang eopl
;; Gramática antes
; <fnc-exp>  ::= 'FNC <natural> (<and-exp>)
; <and-exp>  ::= <clausula> | ('and-exp <clausula> {'or <clausula>}*)
; <clausula> ::= <literal> | ('or-exp <literal> {'or <literal>}*)
; <literal>  ::= ('literal <entero-no-cero>)

;; Gramática despues
; <fnc-exp>  ::= ('FNC <natural> (<and-exp>))
; <and-exp>  ::= <clausula> | (<clausula> {'and <clausula>}+)
; <clausula> ::= <literal> | (<literal> {'or <literal>}+)
; <literal>  ::= <entero-no-cero>

;______________________________________;
;Gramatica actualll
; <fnc-exp>  ::=  ('FNC <natural> <and-exp>)
; <and-exp>  ::=  <clausula>
;            ::=  (<clausula> 'and <and-exp>)
; <clausula> ::=  (<or-exp>)
; <or-exp>   ::=  <literal>
;            ::=  <literal> 'or <or-exp>
; <literal>  ::=  <entero-no-cero>

;intenta representar esto
; (FNC 1 (1)) 
; (FNC 2 (1 or 2 or -1))

; (FNC 2 ((1 or 2 or -1) and (2 or 1 or 2)))



; Definamos la interfaz, nuestro contrato
; Constructores
; fnc-exp: entero-no-cero x <and-exp> -> fnc-exp
; and-simple: clausula -> and-exp
; and-compuesto: clausula x and-exp -> and-exp
; clausula-exp: or-exp -> clausula
; or-simple: literal -> or-exp
; or-compuesto: literal x or-exp -> or-exp
; lit-exp: entero-no-cero -> literal


(define lit-exp
  (lambda (number)
    (list 'literal number)
    )
  )

(define or-simple
  (lambda (literal)
    (list 'or-simple literal)
    )
  )

(define or-compuesto
  (lambda (literal or-exp)
    (list 'or-compuesto literal or-exp)))

; usos 
(or-compuesto (lit-exp 2) 
               (or-compuesto (lit-exp 1) 
                             (or-simple (lit-exp 2))))
(or-simple (lit-exp 2))

(or-compuesto (lit-exp 2) (lit-exp 2))
;------

(define clausula-exp
  (lambda (or-exp)
     (list 'clausula-exp or-exp)
    )
  )

(clausula-exp (or-simple (lit-exp 2)))

(define and-simple
  (lambda (clausula)
    (list 'and-simple clausula)
    )
  )

(and-simple (clausula-exp (or-simple (lit-exp 2))))


(define and-compuesto
  (lambda (clausula and-exp)
    (list 'and-compuesto clausula and-exp)
    )
  )

(and-compuesto (clausula-exp (or-simple (lit-exp 2))) (and-simple (clausula-exp (or-simple (lit-exp 2))))
)

(define fnc-exp
  (lambda (entero-no-cero and-exp)
    (list 'FNC entero-no-cero and-exp)
    )
  )

(fnc-exp 3 (and-compuesto (clausula-exp (or-simple (lit-exp 2))) (and-simple (clausula-exp (or-simple (lit-exp 2))))
))
(fnc-exp 2 (and-simple (clausula-exp (or-simple (lit-exp 2)))))
(fnc-exp 2 
  (and-compuesto 
    (clausula-exp (or-compuesto (literal 1) (or-simple (literal 2))))
    (and-simple (clausula-exp (or-simple (literal -1))))))




;;;; Implementación Basada en Listas ;;;;

;;; Constructores

;; Constructor de Literal
(define literal
  (lambda (value)
    (list 'literal value)
    ))

;; Constructor de OR (clausula)
(define or-exp
  (lambda (var-list)
    (define add-ors
      (lambda (var-list)
        (if (null? (cdr var-list))
            (list (car var-list))
            (cons (car var-list)(cons 'or (add-ors (cdr var-list))))
            )
        )
      )
    (cond
      [(null? var-list) (eopl:error "Una clasúla debe contener al menos un literal")]
      [(null? (cdr var-list)) (list 'or-exp (car var-list))]
      [else (cons 'or-exp (add-ors var-list))]
      )
     )
  )

;; Constructor de AND (lista de clausulas - or-exp)
(define and-exp
  (lambda (or-list)
    (define add-ands
      (lambda (var-list)
        (if (null? (cdr var-list))
            (list (car var-list))
            (cons (car var-list)(cons 'and (add-ands (cdr var-list))))
            )
        )
      )
    (cond
      [(null? or-list) (eopl:error "Una expresión AND debe contener al menos una expresion")]
      [(null? (cdr or-list)) (list 'and-exp (car or-list))]
      [else (cons 'and-exp (add-ands or-list))]
      )
     )
  )

;; Constructor de fnc
; and-exp : estrctura and 
;(define fnc-exp
;  (lambda (and-exp)
 ;   (list 'FNC count-vars(and-exp) and-exp)
;    )
 ; )

;; Funciones auxiliares

;; count-vars :
;; Proposito:
;; and-exp -> int : Procedimiento que para una expresión and retorna el número de variables
;; booleanas distintas que se emplean
; <and-exp>  ::= <clausula> | ('and-exp <clausula> {'or <clausula>}*)
; <clausula> ::= <literal> | ('or-exp <literal> {'or <literal>}*)
; <literal>  ::= ('literal <entero-no-cero>)

(define count-vars
  (lambda (exp)
    (cond
      [(or (null? exp) (not (list? exp))) 0]
      [(eqv? (car exp) 'and-exp) (sum-list (map count-vars (cdr exp)))]
      [(eqv? (car exp) 'or-exp) (sum-list (map count-vars (cdr exp)))]
      [(eqv? (car exp) 'literal) 1]
      [else 0]
      )))


;; sum-list :
;; Proposito:
;; int-list -> int : Procedimiento que para una lista de enteros retorna el valor de la suma de
;;                   todos sus elementos

(define sum-list
  (lambda (list)
    (if (null? list)
        0
        (+ (car list) (sum-list (cdr list)))
        )
    )
  )


;; is-in? :
;; Proposito:
;; Scheme-Value x List -> Bool : Procedimiento que para una lista y un valor, retorna #t si el
;;                               valor hace parte de los elementos de la lista y #f de lo contrario
(define is-in?
  (lambda (val list)
    (if (null? list)
        #f
        (or (eqv? (car list) val) (is-in? val (cdr list)))
        )
    )
  )

;;; Predicados

(define fnc-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'FNC))))

(define or-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'or-exp))))

(define and-exp?
  (lambda (exp)
    (and (list? exp)         
         (not (null? exp))   
         (eqv? (car exp) 'and-exp))))


;;; Extractores

;; Variables de la fnc
(define fnc->var
  (lambda (exp)
    (cadr exp)))

;; Clausulas de la fnc
(define fnc->clausulas
  (lambda (exp)
  (caddr exp)))

;; Variables de una clausula OR
(define or->varlist
  (lambda (clausula)
    (cadr clausula)))


;; Variables de una lista de clausulas AND
(define and->clausulas
  (lambda (and-exp)
    (cadr and-exp)))

;;; Definición de Instancias

;; Instancia 1 - FNC 2 ((1 or 2) and (-1) and (-2))


;; Instancia 2 - FNC 4 (1 or -2 or 3 or 4) and (-2 or 3) and (-1 or -2 or -3) and (3 or 4) and (2))


;; Instancia 3 - FNC 3 ((3 or -1) and (-3) and (-2))



;;; Ejemplos

;; Probando predicados (innecesario pero quería ajajja)
(fnc-exp? '())
(fnc-exp? '())
(fnc-exp? '())

;; Probando extractores
(fnc->var '(FNC 0))
(fnc->var '(FNC 0))
(fnc->clausulas '(FNC 0 '()))
(or->varlist '('or-exp '()))
(or->varlist '('or-exp '()))




