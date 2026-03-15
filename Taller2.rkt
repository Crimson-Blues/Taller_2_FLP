#lang eopl

;;; Gramática BNF
;<fnc-exp>  ::= (FNC <natural> <and-exp>)
;<and-exp>  ::= (and-simple <or-exp>) | (and-compuesto <or-exp> <and-exp>)
;<or-exp>   ::= (or-simple <literal>) | (or-compuesto <literal> <or-exp>)
;<literal>  ::= (lit-exp <entero-no-cero>)

; Función auxiliar (predicado) para validar naturales
(define natural?
  (lambda (x)
    (and (integer? x)
         (>= x 0))))

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
    (if (natural? natural)
        (list 'FNC natural and-exp)
        (eopl:error "El número de variables debe ser un número natural (>= 0)"))))


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
; Datatype de literal
(define-datatype dt-literal dt-literal? ; No puedo colocar literal pq genera error debido a q ya fue definido
  (dt-lit-exp
   (num integer?)))

; Datatype para or-exp
(define-datatype dt-or-exp dt-or-exp?
  (dt-or-simple (literal dt-literal?))
  (dt-or-compuesto (literal dt-literal?)
                   (resto dt-or-exp?)))

; Datatype para and-exp
(define-datatype dt-and-exp dt-and-exp?
  (dt-and-simple (or-exp dt-or-exp?))
  (dt-and-compuesto (or-exp dt-or-exp?)
                    (resto dt-and-exp?)))

; Datatype para fnc-exp
(define-datatype dt-fnc-exp dt-fnc-exp?
  (FNC
   (vars natural?)
   (and-exp dt-and-exp?)))


;; Definición de Instancias

; Instancia 1 - FNC 2 ((1 or 2) and (-1) and (-2))
(define datatype-instancia-1
  (FNC 2
       (dt-and-compuesto (dt-or-compuesto (dt-lit-exp 1) (dt-or-simple (dt-lit-exp 2)))
                         (dt-and-compuesto (dt-or-simple (dt-lit-exp -1))
                                           (dt-and-simple (dt-or-simple (dt-lit-exp -2)))))))

; Instancia 2 - FNC 3 ((3 or -1) and (-3) and (-2))
(define datatype-instancia-2
  (FNC 3
       (dt-and-compuesto (dt-or-compuesto (dt-lit-exp 3) (dt-or-simple (dt-lit-exp -1)))
                         (dt-and-compuesto (dt-or-simple (dt-lit-exp -3))
                                           (dt-and-simple (dt-or-simple (dt-lit-exp -2)))))))

; Instancia 3 - FNC 4 ((1 or -2 or 3) and (-1 or 4) and (2))
(define datatype-instancia-3
  (FNC 4
       (dt-and-compuesto (dt-or-compuesto (dt-lit-exp 1)
                                          (dt-or-compuesto (dt-lit-exp -2) (dt-or-simple (dt-lit-exp 3))))
                         (dt-and-compuesto (dt-or-compuesto (dt-lit-exp -1) (dt-or-simple (dt-lit-exp 4)))
                                           (dt-and-simple (dt-or-simple (dt-lit-exp 2)))))))


;; Ejemplos de utilización

; Probando predicados automaticos
(dt-fnc-exp? datatype-instancia-1)
(dt-fnc-exp? datatype-instancia-2)
(dt-fnc-exp? datatype-instancia-3)
(dt-fnc-exp? 5)
(dt-literal? (dt-lit-exp -4))
(dt-or-exp? (dt-or-simple (dt-lit-exp 2)))

; Extrayendo info 

; Extraer número de variables en la instancia-3
(cases dt-fnc-exp datatype-instancia-3
   (FNC (vars and-exp) vars))

; Extraer el número de un literal directo
(cases dt-literal (dt-lit-exp 8)
  (dt-lit-exp (num) num))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;parser basoado en listas (PARSEBNF)

(define PARSE-OR-EXP
  (lambda (exp)
    (cond
      ((null? (cdr exp))
       (or-simple (literal (car exp))))
      ((eqv? (cadr exp) 'or)
       (or-compuesto (literal (car exp))
                     (PARSE-OR-EXP (cddr exp))))
      (else
       (eopl:error "Sintaxis inválida en expresión OR")))))

; Parser recursivo para arreglos de AND; '((1 or 2) and (3))
(define PARSE-AND-EXP
  (lambda (exp)
    (cond
      ((null? (cdr exp)) 
       (and-simple (PARSE-OR-EXP (car exp))))
      ((eqv? (cadr exp) 'and)
       (and-compuesto (PARSE-OR-EXP (car exp))
                      (PARSE-AND-EXP (cddr exp))))
      (else
       (eopl:error "Sintaxis inválida en expresión AND")))))

; Parser principal para instancias FNC (PARSEBNF)
(define PARSEBNF
  (lambda (exp)
    (if (and (list? exp) (not (null? exp)) (eqv? (car exp) 'FNC))
        (fnc-exp (cadr exp)
                 (PARSE-AND-EXP (caddr exp)))
        (eopl:error "Sintaxis inválida para FNC"))))

;; Pruebas del parser
(PARSEBNF '(FNC 3 ((1 or 2) and (-1) and (-2))))
(PARSEBNF '(FNC 4 ((1 or -2 or 3) and (-1 or 4) and (2))))
(PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
(-1 or -2 or -3) and (3 or 4) and ( 2 ) )))

;;;;;;;;;;;;;;;;
;;; (UNPARSEBNF) 

; Unparser recursivo para OR
(define UNPARSE-OR-EXP
  (lambda (ast)
    (cond
      ((or-simple? ast)
       (list (lit-exp->num (or-simple->literal ast))))
      ((or-compuesto? ast)
       (cons (lit-exp->num (or-compuesto->literal ast))
             (cons 'or (UNPARSE-OR-EXP (or-compuesto->or-exp ast)))))
      (else
       (eopl:error "AST inválido para expresión OR")))))

; Unparser recursivo para AND
(define UNPARSE-AND-EXP
  (lambda (ast)
    (cond
      ((and-simple? ast)
       (list (UNPARSE-OR-EXP (and-simple->or-exp ast))))
      ((and-compuesto? ast)
       (cons (UNPARSE-OR-EXP (and-compuesto->or-exp ast))
             (cons 'and (UNPARSE-AND-EXP (and-compuesto->and-exp ast)))))
      (else
       (eopl:error "AST inválido para expresión AND")))))

; Unparser principal (UNPARSEBNF)
(define UNPARSEBNF
  (lambda (ast)
    (if (fnc-exp? ast)
        (list 'FNC 
              (fnc->var ast) 
              (UNPARSE-AND-EXP (fnc->and-exp ast)))
        (eopl:error "El AST no es una FNC válida"))))

;; Pruebas
(UNPARSEBNF '(FNC
 4
 (and-compuesto
  (or-compuesto
   (lit-exp 1)
   (or-compuesto (lit-exp -2) (or-simple (lit-exp 3))))
  (and-compuesto
   (or-compuesto (lit-exp -1) (or-simple (lit-exp 4)))
   (and-simple (or-simple (lit-exp 2)))))))
(UNPARSEBNF (PARSEBNF '(FNC 3 ((1 or 2) and (-1) and (-2)))))
(UNPARSEBNF instancia-1)
(UNPARSEBNF (PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
(-1 or -2 or -3) and (3 or 4) and ( 2 ) )))
)


