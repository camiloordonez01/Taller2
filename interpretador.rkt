#lang eopl
;******************************************************************************************
;;;;; Interpretador Taller 2

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      <a-programa (exp)>
;;  <expresion>     ::= <number>
;;                      <lit-exp (dato)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <primitiva> ({<expresion>}*(,))
;;                      <primapp-exp (prim rands)>
;;  <primitive>     ::= + | - | * | 

;******************************************************************************************


;******************************************************************************************
;Especificación Léxica
;En especificacionLexica es una parte de la especificacion del lenguaje el consiste en proveer
;informacion acerca de la estrutura de una secuencia de caracteres. especificamos el espacio en blanco la cual es ignorada,
;la iniciacion de un comentario tambien es ignorada, el identificador es la secuencia de letra y numeros que son las que se toman
;en cuenta para la generacion de arbol de sintaxis abstracta , el number indentifica los caracteres de tipo numero yasea positivo
;o negativo, ....
(define especificacionLexica
'(
  (espacio (whitespace) skip)
  (comentario ("%" (arbno (not #\newline))) skip)
  (identificador (letter (arbno (or letter digit "?"))) symbol)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (number (digit (arbno digit)"."digit (arbno digit))number)
  (number ("-"digit (arbno digit)"."digit (arbno digit))number)))

;Especificación Sintáctica (gramática)
;En especificacionGramatical es donde vamos a indicar la forma en la cual se deben organizar la unidades definicada en la
;especificacionLexica. Para progama se define la forma que se va evaluar cada expesion comenzando con un programa al cual
;toma como entrada un expresion y devuelve a-programa que es el resultado de las operaciones; Una expresion puede ser de varios
;tipo: number que es un digito, identificador es un simbolo de scheme, las primitivas son las operaciones que puede hacer
;nuestro programa las cuales son de tipo: sum-prim(+) que es la suma de dos numero, rest-prim(-)que es la resta de dos numeros
; y mult-prim(*) que es la multiplicacion de dos numeros
(define especificacionGramatical
  '(
    (programa (expresion) a-programa)
    (expresion (number) lit-exp)
    (expresion (identificador) var-exp)
    (expresion
     (primitiva "(" (separated-list expresion ",")")")
     primapp-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") rest-prim)
    (primitiva ("*") mult-prim)))

;Construidos automáticamente:

(sllgen:make-define-datatypes especificacionLexica especificacionGramatical)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacionLexica especificacionGramatical)))



;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser especificacionLexica especificacionGramatical))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner especificacionLexica especificacionGramatical))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      especificacionLexica
      especificacionGramatical)))

;*******************************************************************************************

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))))) 

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (+ (car args) (cadr args)))
      (rest-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************