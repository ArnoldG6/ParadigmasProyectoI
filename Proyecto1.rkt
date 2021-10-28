#lang racket
;
; ===============================================
;
;(c) 2021
; version 1.0.0 2021-11-06
;
;-----------------------------------------------
; EIF400 Paradigmas de Programación
; 2do ciclo 2021, grupo 01
; Proyecto 1
;
; A00148103 González Carvajal, Miguel
; 117620480 González Quesada, Arnoldo
; 117960315          Ocampo Marín, Victor
;
; ===============================================
;
; Inserte código místico xd

(define (alelo_rand)  ;Genera un número random del conjunto {0,1}.
    (+ 0 (random 2)))

	
(define generar_individuo_t ;Genera un individuo con una cadena genética de n alelos, usando "tail recursion".
  (lambda (m n L)
    (if (= m n)
        L
        (cons (alelo_rand) (generar_individuo_t(+ m 1) n L)); 
		
    )
  )
)
(define (generar_individuo) ;Genera un individuo de cádena genética con n = 10 alelos por defecto.
    (generar_individuo_t 0 10 '())
)
(define generar_individuos_t ;Genera n individuos de cadena genética de 10 alelos de manera randomizada, usando "tail recursion".
  (lambda (m n L)
    (if (= m n)
        L
        (cons (generar_individuo) (generar_individuos_t(+ m 1) n L))
    )
  )
)
(define generar_individuos ;Genera n individuos de cádena genética de 10 alelos de manera randomizada.
  (lambda (n)
    (generar_individuos_t 0 n '())
  )
)


(define func_obj_t ;Parámetros: (I: individuo, acumulador de resultado).
  (lambda (I r)
    (if (equal? '() I)
        r
        (func_obj_t (cdr I) (+ r (car I)))
    )
  )
)

(define func_obj ;Función objetivo para cada uno de los individuos.
  (lambda (I)
	(func_obj_t I 0)
  )
)


; (define a '(0 0 0 1 0 1 1 1 0 0))
; (define b '(0 0 0 0 1 1 1 0 1 1))
; (define c '(1 0 1 0 0 1 1 1 0 1))
; (define d '(1 1 1 0 1 0 0 1 0 1))
; (define e '(1 1 1 1 1 1 1 1 1 1))
; (printf "Probando con 'a: '~s.\n" (func_obj a))
; (printf "Probando con 'b: '~s.\n" (func_obj b))
; (printf "Probando con 'c: '~s.\n" (func_obj c))
; (printf "Probando con 'd: '~s.\n" (func_obj d))
; (printf "Probando con 'e: '~s.\n" (func_obj e))
; (generar_individuos 5)



