#lang eopl

;; Declaración de uso de IA:
;; Se utilizó IA para: Mejorar la redacción de documentación y comprender
;; algunos mensajes de error del intérprete
;; Se declara que todos los integrantes comprenden y pueden explicar
;; completamente cada soluci´on implementada.

;;; ==========================================================
;;;                    INSTANCIAS SAT
;;; ==========================================================

;;; Gramática BNF
;<fnc-exp>  ::= (FNC <natural> <and-exp>)
;<and-exp>  ::= (and-simple <or-exp>) | (and-compuesto <or-exp> <and-exp>)
;<or-exp>   ::= (or-simple <literal>) | (or-compuesto <literal> <or-exp>)
;<literal>  ::= (lit-exp <entero-no-cero>)

;; natural? : Num -> Bool
;; Propósito: Verifica si un valor es un número entero mayor a cero.
(define natural?
  (lambda (x)
    (and (integer? x)
         (> x 0))))

;;; ==========================================================
;;; 1.1 IMPLEMENTACIÓN BASADA EN LISTAS
;;; ==========================================================

;;; --- Constructores ---

;; literal : Int -> List
;; Propósito: Construye el nodo AST de un literal, validando que sea un entero distinto de cero.
(define literal
  (lambda (num)
    (if (and (integer? num) (not (zero? num)))
        (list 'lit-exp num)
        (eopl:error "El literal debe ser un entero diferente de cero"))))

;; or-simple : literal -> or-exp
;; Propósito: Construye una expresión OR que contiene un único literal.
(define or-simple
  (lambda (literal)
    (list 'or-simple literal)))

;; or-compuesto : literal x or-exp -> or-exp
;; Propósito: Construye una expresión OR compuesta por un literal
;; seguido de otra expresión OR.
(define or-compuesto
  (lambda (literal or-exp)
    (list 'or-compuesto literal or-exp)))

;; and-simple : or-exp -> and-exp
;; Propósito: Construye una expresión AND que contiene una sola cláusula OR.
(define and-simple
  (lambda (or-exp)
    (list 'and-simple or-exp)))

;; and-compuesto : or-exp x and-exp -> and-exp
;; Propósito: Construye una expresión AND compuesta por una cláusula OR
;; seguida de otra expresión AND.
(define and-compuesto
  (lambda (or-exp and-exp)
    (list 'and-compuesto or-exp and-exp)))

;; fnc-exp : Nat x and-exp -> fnc-exp
;; Propósito: Construye el nodo raíz del AST que representa una fórmula
;; en Forma Normal Conjuntiva (FNC). El número natural indica la cantidad
;; de variables proposicionales utilizadas en la fórmula.
(define fnc-exp
  (lambda (natural and-exp)
    (if (natural? natural)
        (list 'FNC natural and-exp)
        (eopl:error "El número de variables debe ser un número natural mayor a 0"))))


;;; --- Predicados ---
;; Propósito: Verifican si una expresión corresponde a una determinada
;; construcción del AST, identificándola por su etiqueta.

(define fnc-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'FNC))))

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

(define lit-exp?
  (lambda (exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? (car exp) 'lit-exp)
         (integer? (cadr exp))
         (not (zero? (cadr exp))))))


;;; --- Extractores ---
;; Propósito: Permiten acceder a los componentes internos de cada nodo
;; del Árbol de Sintaxis Abstracta (AST).

(define fnc->var
  (lambda (exp)
    (cadr exp)))

(define fnc->and-exp
  (lambda (exp)
    (caddr exp)))

(define and-compuesto->or-exp
  (lambda (exp)
    (cadr exp)))

(define and-compuesto->and-exp
  (lambda (exp)
    (caddr exp)))

(define and-simple->or-exp
  (lambda (exp)
    (cadr exp)))

(define or-compuesto->literal
  (lambda (exp)
    (cadr exp)))

(define or-compuesto->or-exp
  (lambda (exp)
    (caddr exp)))

(define or-simple->literal
  (lambda (exp)
    (cadr exp)))

(define lit-exp->num
  (lambda (exp)
    (cadr exp)))


;;; --- Definición de Instancias y Pruebas ---

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

;;; ==========================================================
;;; 1.2 IMPLEMENTACIÓN BASADA EN DATATYPES
;;; ==========================================================

;;; --- Definición de Datatypes ---
(define-datatype dt-literal dt-literal? 
  (dt-lit-exp
   (num integer?)))

(define-datatype dt-or-exp dt-or-exp?
  (dt-or-simple (literal dt-literal?))
  (dt-or-compuesto (literal dt-literal?)
                   (resto dt-or-exp?)))

(define-datatype dt-and-exp dt-and-exp?
  (dt-and-simple (or-exp dt-or-exp?))
  (dt-and-compuesto (or-exp dt-or-exp?)
                    (resto dt-and-exp?)))

(define-datatype dt-fnc-exp dt-fnc-exp?
  (FNC
   (vars natural?)
   (and-exp dt-and-exp?)))


;;; --- Definición de Instancias y Pruebas ---

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

; Probando predicados automaticos
(dt-fnc-exp? datatype-instancia-1)
(dt-fnc-exp? datatype-instancia-2)
(dt-fnc-exp? datatype-instancia-3)
(dt-fnc-exp? 5)
(dt-literal? (dt-lit-exp -4))
(dt-or-exp? (dt-or-simple (dt-lit-exp 2)))

; Extrayendo información 

; Extraer número de variables en la instancia-3
(cases dt-fnc-exp datatype-instancia-3
   (FNC (vars and-exp) vars))

; Extraer el número de un literal directo
(cases dt-literal (dt-lit-exp 8)
  (dt-lit-exp (num) num))


;;; ==========================================================
;;; 2.1 PARSER BASADO EN LISTAS (PARSEBNF)
;;; ==========================================================

;; PARSE-OR-EXP : List -> or-exp
;; Propósito: Transforma recursivamente una lista plana separada por 'or' en un AST de tipo or-exp.
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

;; PARSE-AND-EXP : List -> and-exp
;; Propósito: Transforma recursivamente una lista plana separada por 'and' en un AST de tipo and-exp.
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

;; PARSEBNF : List -> fnc-exp
;; Propósito: Función principal del parser. Recibe una representación en listas de una
;; fórmula FNC y construye su correspondiente Árbol de Sintaxis Abstracta (AST)
(define PARSEBNF
  (lambda (exp)
    (if (and (list? exp) (not (null? exp)) (eqv? (car exp) 'FNC))
        (fnc-exp (cadr exp)
                 (PARSE-AND-EXP (caddr exp)))
        (eopl:error "Sintaxis inválida para FNC"))))


;;; --- Pruebas Parser---
(PARSEBNF '(FNC 3 ((1 or 2) and (-1) and (-2))))
(PARSEBNF '(FNC 4 ((1 or -2 or 3) and (-1 or 4) and (2))))
(PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
(-1 or -2 or -3) and (3 or 4) and ( 2 ) )))


;;; ==========================================================
;;; 2.2 UNPARSER BASADO EN LISTAS (UNPARSEBNF)
;;; ==========================================================

;; UNPARSE-OR-EXP : or-exp -> List
;; Propósito: Convierte un AST de tipo or-exp de vuelta a una lista legible.
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

;; UNPARSE-AND-EXP : and-exp -> List
;; Propósito: Convierte un AST de tipo and-exp de vuelta a una lista legible.
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

;; UNPARSEBNF : fnc-exp -> List
;; Propósito: Convierte una expresión representada como AST en su
;; representación original basada en listas.
(define UNPARSEBNF
  (lambda (ast)
    (if (fnc-exp? ast)
        (list 'FNC 
              (fnc->var ast) 
              (UNPARSE-AND-EXP (fnc->and-exp ast)))
        (eopl:error "El AST no es una FNC válida"))))


;;; --- Pruebas Unparser---
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


;;; ==========================================================
;;; 3. EVALUACIÓN DE INSTANCIAS SAT
;;; ==========================================================

;; probarEXP : fnc-exp x List(Bool) -> Bool
;; Propósito: Evalúa el valor de verdad de una fórmula en FNC
;; dada una asignación de valores booleanos para sus variables.
;; Reemplaza los literales por su valor.
(define probarEXP
  (lambda (exp vars)
    (cond
      [(fnc-exp? exp) (probarEXP (fnc->and-exp exp) vars)]
      [(and-compuesto? exp) (and (probarEXP (and-compuesto->or-exp exp) vars)
                                 (probarEXP (and-compuesto->and-exp exp) vars))]
      [(and-simple? exp) (probarEXP (and-simple->or-exp exp) vars)]
      [(or-compuesto? exp) (or (probarEXP (or-compuesto->literal exp) vars)
                                 (probarEXP (or-compuesto->or-exp exp) vars))]
      [(or-simple? exp) (probarEXP (or-simple->literal exp) vars)]
      [(lit-exp? exp) (let (
                            [bool-var (list-ref vars (- (abs (lit-exp->num exp)) 1))]
                            [num (lit-exp->num exp)]
                            )
                        (if (> num 0)
                            bool-var
                            (not bool-var))
                            
                        )
                            ]
      [else (eopl:error "Expresión no es una FNC válida")]
      )
    )
  )

;; Ejemplos de utilización
;; Formula insatisfactible
(probarEXP instancia-1 '(#t #t))
(probarEXP instancia-1 '(#t #f))

;; Fórmula satisfactible
(probarEXP instancia-2 '(#f #f #f))
(probarEXP instancia-2 '(#f #t #f))

;; bool-comb : natural -> List(List(Bool))
;; Proposito: Recibe un numero natural n y retorna una lista de n-tuplas de valores booleanos
;; correspondiente a todos las permutaciones posibles de valores de verdad.

;; <natural>  ::= <zero> | sucesor(<natural>)

(define bool-comb
  (lambda (n)
    (if (zero? n)
        '(())
        (let (
              [cons-bool (lambda (bool)
                           (lambda (list)
                             (cons bool list)))])
          (append
           (map (cons-bool #t) (bool-comb (- n 1)))
           (map (cons-bool #f) (bool-comb (- n 1))))
          )
        )
    )
  )

;; Ejemplos de utilización
;; Caso base n = 0
(bool-comb 0)

;; Casos de uso común
(bool-comb 3)
(bool-comb 4)

;; EVALUARSAT : fnc-exp -> List
;; Propósito: Determina si una fórmula en FNC es satisfactible o
;; insatisfactible. Si existe una asignación de valores de verdad que
;; hace verdadera la fórmula, retorna 'satisfactible junto con dicha
;; asignación. En caso contrario, retorna 'insatisfactible.

(define EVALUARSAT
  (lambda (fnc-exp)
    (define EVALUARACC
      (lambda (fnc-exp bool-list)
       (if (null? bool-list)
       (list 'insatisfactible bool-list)
       (let ([test (probarEXP fnc-exp (car bool-list))])
         (cond
           [(eqv? test #t) (list 'satisfactible (car bool-list))]
           [(eqv? test #f) (EVALUARACC fnc-exp (cdr bool-list))]
           )
         ))
        )
      )
    (EVALUARACC fnc-exp (bool-comb (fnc->var fnc-exp)))
    )
  )

;; Ejemplos de utilización
;;; Instancias ya creadas

;; Fórmula insatisfactible
(EVALUARSAT instancia-1)

;; Fórmula satisfactible
;; De tres variables
(EVALUARSAT instancia-2)

;; De cuatro variables
(EVALUARSAT instancia-3)

;;; Usos con parser
(EVALUARSAT (PARSEBNF '(FNC 4 ((1 or -2 or 3 or 4) and (-2 or 3) and
(-1 or -2 or -3) and (3 or 4) and ( 2 ) ))))
