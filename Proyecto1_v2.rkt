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
#|
El Algoritmo Genético Simple
BEGIN /* Algoritmo Genetico Simple */
	Generar una poblacion inicial.
	Computar la funcion de evaluacion de cada individuo.
	WHILE NOT Terminado DO
	BEGIN /* Producir nueva generacion */
		FOR Tamano˜ poblacion/2 DO
		BEGIN /*Ciclo Reproductivo */
			Seleccionar dos individuos de la anterior generacion,
			para el cruce (probabilida de seleccion proporcional
			a la funcion de evaluacion del individuo).
			Cruzar con cierta probabilidad los dos
			individuos obteniendo dos descendientes.
			Mutar los dos descendientes con cierta prdobabilidad.
			Computar la funcion de evaluacion de los dos
			descendientes mutados.
			Insertar los dos descendientes mutados en la nueva generacion.
		END
IF la poblacion ha convergido THEN
	Terminado := TRUE
END
END
|#
;=========================FUNCIONES AUXILIARES===========================
;==================SUMA======================
(define suma_t  ;Retorna la suma de los elementos de una lista
	(lambda (L acum)
		(if (equal? '() L)
			acum
			(suma_t (cdr L) (+ acum (car L)))
		)
	)
)
(define (suma L) ;Retorna la suma de los elementos de una lista, inicializando acum en 0
	(suma_t L 0)
)
(printf "Prueba de (suma'(1 2 3)): ~s.\n" (suma '(1 2 3)))
(define (dividir L num) ;Toma una lista y generar una partición desde 0 a num
	(cons
		(takef L (lambda (n) (<= n num)))
		(list (dropf L (lambda (n) (<= n num))))
		;(dropf L (lambda (n) (<= n num)))
	)
)
(printf "Prueba de (dividir '(1 2 3 4 5 6) 3): ~s.\n" (dividir '(1 2 3 4 5 6) 3))
; Entra '((1 2 3) 4 5 6), 
; (define (sub_dividir_t valores L) ;Toma la primera lista de salida de (dividir) y la convierte en sublistas
	; (if (equal? '() L)
		
	; )
; )
; (define (sub_dividir valores) ;Toma la primera lista de salida de (dividir) y la convierte en sublistas

; )
;=========================GENERACION DE POBLACION========================
;Inicializar grupos
; Si entra (1 2 3 4 5 6 7) con can_grupos = 4 sale ((1) (2) (3) (4) 5 6 7)
; (define (ini_grupos_t can_grupos valores) 

; )
; (define (ini_grupos
; )
; Genera una población de tam_pob individuos, conformados por can_grupos, dentro del conjunto de valores
; (define generar_ind_t
	; (lambda (can_grupos valores ind)

	; )
; )
; (define (generar_ind can_grupos valores)
	; (generar_ind_t can_grupos valores '()) ;ind se inicializa en '()
; )

; (define generar_poblacion_t
	; #|
	; tam_pob: Define la cantidad de individuos por cada generación.
	; can_grupos: Cantidad de grupos que se van a hacer dentro de cada individuo.
	; valores: Lista de números que contiene el conjunto númerico para generar individuos.
	; |#
	; (lambda (tam_pob can_grupos valores)
		
	; )
; )
; (define generar_poblacion 
	; #|
	; tam_pob: Define la cantidad de individuos por cada generación.
	; can_grupos: Cantidad de grupos que se van a hacer dentro de cada individuo.
	; valores: Lista de números que contiene el conjunto númerico para generar individuos.
	; |#
	; (lambda (tam_pob can_grupos valores)
		
	; )
; )
;==========================FUNCIÓN OBJETIVO==============================
;=========================SELECCIÓN DE POBLACION=========================
;=========================CRUCE DE POBLACION=============================
;=========================MUTACIÓN DE POBLACION==========================
;======================NUEVA GENERACIÓN DE POBLACION=====================
;========================CONVERGENCIA DE POBLACIÓN=======================
;===================ALGORITMO GENÉTICO===================================
(define resolver
    #|
	Parámetros:
	max_gen: Define el número máximo de generaciones.
	tam_pob: Define la cantidad de individuos por cada generación.
	elit: #t si se aplica elitismo, #f si no.
	can_grupos: Cantidad de grupos que se van a hacer dentro de cada individuo.
	valores: Lista de números que contiene el conjunto númerico para generar individuos.
	|#
	(lambda (max_gen tam_pob elit can_grupos valores)
		"resolver: NO IMPLEMENTADO"
	)
)

; (resolver 500 30 #t 4 '(30 60 90 25 20 15 16 120 200 43 18 30 30))
; (resolver  500 6 #t 4 '(1 2 3 5 6 9 32 1 2 5 12 31 15))
(exit)
