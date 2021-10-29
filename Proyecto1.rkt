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


;=========================INDIVIDUOS=========================
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


(define func_obj_t ;Parámetros: (I: individuo, r: acumulador de resultado).
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
;=========================POBLACIÓN=========================
						;Esta función inicializa una población de individuos de tamaño n y;
						;genera la lista con los pares (x,y), en donde x:individuo,y: el valor
						; de su función objetivo
						;L: lista resultante.
						;I: Lista de individuos
(define inic_poblacion_t 
  (lambda (L I)
	(if (equal? '() I)
		L
		(cons (list(car I) (func_obj (car I))) (inic_poblacion_t L (cdr I)))
    )
  )
)
(define inic_poblacion
  (lambda (n)
	(inic_poblacion_t '() (generar_individuos n))
  )
)
(define max
  (lambda (x y)
    (if (< x y) 
	   y 
	   x
	)
  )
)
(define individuo_mas_apto ;compara por valor de función objetivo y devuelve al individuo más apto.
  (lambda (x y)
    (if (< (car(cdr x)) (car(cdr y)))
	   x 
	   y
	)
  )
)
(define mejor_individuo_pob_t ;Devuelve el mejor individuo de una lista según la función objetivo.
  (lambda (L i) ;i: individuo 
  	(if (equal? '() L)
		i 
		(mejor_individuo_pob_t (cdr L) (individuo_mas_apto i (car L)))
    )
  )
)
(define mejor_individuo_pob
  (lambda (L)
	(mejor_individuo_pob_t L (car L))
  )
)
(define imp_poblacion
  (lambda (L)
	(printf "-------------------------Población sin ordenar-------------------------\n")
	(printf "~s \n." L)
	(printf "--------------------------Población ordenada---------------------------\n")
	(printf "~s.\n" (ordenar_poblacion L))
  )
)
(define (inic_poblacion_usuario) ;Esta función inicializa una población de individuos de tamaño n.
	(printf "Digite el número de individuos que conforman la población: ")
	(imp_poblacion (inic_poblacion (read)))
)

;(define f '(((1 0 1 0 1 0 1 0 1 0) 5) ((0 1 0 0 1 0 0 0 0 0) 2) ((1 0 1 1 0 0 0 1 0 0) 4) ((1 1 1 0 1 0 1 0 0 0) 5) ((1 0 0 0 0 1 1 0 1 0) 4)))
;f
; (mejor_individuo_pob f)

(define eliminar_mejor_ind ;Desplaza el individuo más apto de la lista, y retorna la lista inicial sin dicho individuo.
	(lambda (L)
		(remove (mejor_individuo_pob L) L)
	)
)
(define ordenar_poblacion_t
	(lambda (R L) ; R: Lista de individuos con peso ordenada. L: Lista de individuos con peso sin ordenar.
		(if (equal? '() L)
			R
			(ordenar_poblacion_t (cons (mejor_individuo_pob L) R) (eliminar_mejor_ind L))
		)	
	)
)
(define ordenar_poblacion
	(lambda (L)
		(ordenar_poblacion_t '() L)
	)
)
(inic_poblacion_usuario)





