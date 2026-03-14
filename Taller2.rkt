#lang eopl

;;; Gramática BNF
;<fnc-exp>  ::= (FNC <natural> <and-exp>)
;<and-exp>  ::= (and-simple <or-exp>) | (and-compuesto <or-exp> <and-exp>)
;<or-exp>   ::= (or-simple <literal>) | (or-compuesto <literal> <or-exp>)
;<literal>  ::= (lit-exp <entero-no-cero>)

;;; Implementación Basada en Listas ;;;

;; Constructores

; Constructor de literal
(define literal
  (lambda (num)
    (if (and (integer? num) (not (zero? num)))
        (list 'lit-exp num)
        (eopl:error "El literal debe ser un entero diferente de cero"))))

; Constructores de OR
(define or-simple
  (lambda (literal)
    (list 'or-simple literal)))

(define or-compuesto
  (lambda (literal or-exp)
    (list 'or-compuesto literal or-exp)))

; Constructores de AND
(define and-simple
  (lambda (or-exp)
    (list 'and-simple or-exp)))

(define and-compuesto
  (lambda (or-exp and-exp)
    (list 'and-compuesto or-exp and-exp)))

; Constructor de FNC
(define fnc-exp
  (lambda (natural and-exp)
    (list 'FNC natural and-exp)))


;; Predicados - Las hice de manera sensilla pq no se piden, pero ya para algo más estricto, tocaría verificar sus elementos

; Predicado de FNC
(define fnc-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'FNC))))

; Predicados de AND 
(define and-compuesto?
  (lambda (exp)
    (and (list? exp)         
         (not (null? exp))   
         (eqv? (car exp) 'and-compuesto))))

(define and-simple?
  (lambda (exp)
    (and (list? exp)         
         (not (null? exp))   
         (eqv? (car exp) 'and-simple))))

; Predicados de OR
(define or-compuesto?
  (lambda (exp)
    (and (list? exp)         
         (not (null? exp))   
         (eqv? (car exp) 'or-compuesto))))

(define or-simple?
  (lambda (exp)
    (and (list? exp)         
         (not (null? exp))   
         (eqv? (car exp) 'or-simple))))

; Predicado de literal
(define lit-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'lit-exp)
         (integer? (cadr exp))
         (not (zero? (cadr exp))))))


;; Extractores

; Extractores de FNC
(define fnc->var
  (lambda (exp)
    (cadr exp)))

(define fnc->and-exp
  (lambda (exp)
    (caddr exp)))

; Extractores de AND compuesto
(define and-compuesto->or-exp
  (lambda (exp)
    (cadr exp)))

(define and-compuesto->and-exp
  (lambda (exp)
    (caddr exp)))

; Extractor de AND simple
(define and-simple->or-exp
  (lambda (exp)
    (cadr exp)))

; Extractores de OR compuesto
(define or-compuesto->literal
  (lambda (exp)
    (cadr exp)))

(define or-compuesto->or-exp
  (lambda (exp)
    (caddr exp)))

; Extractor de OR Simple
(define or-simple->literal
  (lambda (exp)
    (cadr exp)))

; Extractor de literal
(define lit-exp->num
  (lambda (exp)
    (cadr exp)))


;; Definición de Instancias

; Instancia 1 - FNC 2 ((1 or 2) and (-1) and (-2))
(define instancia-1
  (fnc-exp 2
           (and-compuesto (or-compuesto (literal 1) (or-simple (literal 2)))
                          (and-compuesto (or-simple (literal -1))
                                         (and-simple (or-simple (literal -2)))))))

; Instancia 2 - FNC 3 ((3 or -1) and (-3) and (-2))
(define instancia-2
  (fnc-exp 3
           (and-compuesto (or-compuesto (literal 3) (or-simple (literal -1)))
                          (and-compuesto (or-simple (literal -3))
                                         (and-simple (or-simple (literal -2)))))))

; Instancia 3 - FNC 4 ((1 or -2 or 3) and (-1 or 4) and (2))
(define instancia-3
  (fnc-exp 4
           (and-compuesto (or-compuesto (literal 1)
                                        (or-compuesto (literal -2) (or-simple (literal 3))))
                          (and-compuesto (or-compuesto (literal -1) (or-simple (literal 4)))
                                         (and-simple (or-simple (literal 2)))))))


;; Ejemplos

;; Probando predicados

(fnc-exp? instancia-1)
(fnc-exp? instancia-2)
(fnc-exp? instancia-3)
 
(lit-exp? (literal 1))
(lit-exp? (literal -1))
(lit-exp? '(lit-exp 0))

; Verificar si la primera cláusula del AND en instancia-1 es un OR compuesto
(or-compuesto? (and-compuesto->or-exp (fnc->and-exp instancia-1)))

; Verificar si la segunda cláusula de la instancia-2 es un OR simple
(or-simple? (and-compuesto->or-exp (and-compuesto->and-exp (fnc->and-exp instancia-2))))

; Verificar si el final de la instancia-1 cierra correctamente con un AND simple
(and-simple? (and-compuesto->and-exp (and-compuesto->and-exp (fnc->and-exp instancia-1))))

;; Probando extractores

; Extraer número de variables en la instancia-3
(fnc->var instancia-3)

; Extraer primer número de la instancia-2
(lit-exp->num (or-compuesto->literal (and-compuesto->or-exp (fnc->and-exp instancia-2))))

; Extraer último número de la primera cláusula de la instancia-3
(lit-exp->num (or-simple->literal (or-compuesto->or-exp (or-compuesto->or-exp (and-compuesto->or-exp (fnc->and-exp instancia-3))))))

; Extraer el ultimo número de toda la instancia-1
(lit-exp->num (or-simple->literal (and-simple->or-exp (and-compuesto->and-exp (and-compuesto->and-exp (fnc->and-exp instancia-1))))))



;;; Implementación Basada en Datatypes ;;;
