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
; A00148103 González Carvajal, Miguel GRUPO 3PM
; 117620480 González Quesada, Arnoldo GRUPO 3PM
; 117960315 Ocampo Marín, Victor      GRUPO 3PM
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
(define individuo_menos_apto ;compara por valor de función objetivo y devuelve al individuo más apto.
  (lambda (x y)
    (if (< (car(cdr x)) (car(cdr y)))
	   x
	   y
	)
  )
)
(define individuo_mas_apto ;compara por valor de función objetivo y devuelve al individuo menos apto.
  (lambda (x y)
    (if (>= (car(cdr x)) (car(cdr y)))
	   x
	   y
	)
  )
)
(define mejor_individuo_pob_t ;Devuelve el mejor individuo de una lista según la función objetivo.
  (lambda (L i) ;i: individuo
  	(if (equal? '() L)
		i
		(mejor_individuo_pob_t (cdr L) (individuo_menos_apto i (car L)))
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
	(printf "~s.\n." (ordenar_poblacion L))
  )
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
; (define (inic_poblacion_usuario) ;Esta función inicializa una población de individuos de tamaño n.
	; (printf "Digite el número de individuos que conforman la población: ")
	; (imp_poblacion (inic_poblacion (read)))
; )
; (inic_poblacion_usuario)
(define generar_n_grupos_t ;Genera n_grupos grupos de tamaño n_ind, sin ordenarlos.
	(lambda (L n_ind eli n_grupos)
		(if (= n_grupos 0)
			L
			(append (list (inic_poblacion n_ind)) (generar_n_grupos_t L n_ind eli (- n_grupos 1)))
		)
	)
)
(define generar_n_grupos
	(lambda (n_ind eli n_grupos)
		(generar_n_grupos_t '() n_ind eli n_grupos)
	)
)
;=========================SELECCIÓN=========================
(define ordenar_n_grupos_t
	(lambda (R G);G:grupos, R: lista resultante.
		(if (equal? '() G)
			R
			(ordenar_n_grupos_t (cons (ordenar_poblacion (car G)) R) (cdr G))
		)
	)
)
(define ordenar_n_grupos
	(lambda (G)
		(ordenar_n_grupos_t '() G)
	)
)
(define mas_aptos_grupo ;Retorna los dos individuos más aptos de una lista de individuos I contenida en un grupo.
	(lambda (I)
		(list (car (car I)) (car (cdr (car I))))
	)
)
(define mas_aptos_lista_individuos ;Retorna los dos individuos más aptos de una lista de individuos I.
	(lambda (I)
		(list (car I) (car (cdr I)))
	)
)
(define mas_aptos_t ;Retorna una lista con los dos individuos más aptos de cada grupo en una generación. G: Grupos, R: Lista resultante
	(lambda (R G)
		(if (equal? '() G)
			R
			(cons (mas_aptos_lista_individuos (car G)) (mas_aptos_t  R (cdr G)))
		)
	)
)
(define mas_aptos ;Retorna una lista con los dos individuos más aptos de cada grupo en una generación. G: Grupos, R: Lista resultante
	(lambda (G)
		(mas_aptos_t '() G)
	)
)
; (ordenar_n_grupos(generar_n_grupos 2 #t 4)) ;Generación 0

(define (test L) ;Generación 0
	(printf "Generación 0: ~s.\n" L)
	(printf "Selección de los más aptos: ~s.\n" (mas_aptos L))
)
; (test (ordenar_n_grupos(generar_n_grupos 4 #t 3)))


;=========================CRUCE=========================
;aquí va todo lo del cruce xd
(define cruce_primer_ind
  (lambda (J K H)
    (append (take J H) (take-right K (abs(- (length K) H))))
  )
)
(define cruce ;Genera 2 individuos hijos apartir del cruce de 2 individuos padres.
	(lambda (J K n) ;J individuo 1. K: n: número random entre 0 y lenght de J, siendo len J = len K
		(list (cruce_primer_ind J K n) (cruce_primer_ind K J n))
	)
)

;(cruce '((0 1 1 0 1 1 0 0 0 0) (1 1 0 0 0 0 0 1 0 1) (1 0 1 0 0 0 1 0 0 1) (0 0 0 0 0 0 0 0 1 1) (1 1 1 0 0 0 0 1 0 0)) '((0 0 0 0 0 0 0 1 1 0) (0 0 1 0 0 1 0 1 1 1) (1 1 1 1 0 0 1 0 1 0) (1 1 0 1 0 1 0 1 0 0) (1 1 1 0 0 1 0 1 0 0)) (random 1 5))
; (printf "----------------------------------\n")

;(cruce '(0 1 1 0 1 1 0 0 0 0) '(1 1 0 0 0 0 0 1 0 1) (random 0 11))
; (generar_individuos 5)
; (generar_individuos 5)
;=========================MUTACIÓN=========================
;aquí va todo lo de la mutación xd

(define (seleccion l) ;Devuelve un elemento aleatorio de una lista (No se usa xd)
  (list-ref l (random (length l)))
 )

(define seleccion_aleatoria ;Devuelve una posicion aleatoria de una lista
  (lambda (L)
    (list (random (length L)))
    )
 )

(define invertir_valor ;Realiza el cambio de bits para las mutaciones
  (lambda (valor)
    (if (equal? 0 valor)
        1
        0
    )
  )
)


(define (cambia_valor_lista L indices valores [offset 0])
  (cond [(null? indices) L] ; no mas cambios, retorna L
        [(null? L) null]    ; no mas L, retorna null
        [(= (car indices) offset) ; condicion para cambiar
         (cons (car valores)
               (cambia_valor_lista (cdr L) (cdr indices) (cdr valores) (add1 offset)))]
        [else
         (cons (car L)    ; sin condicion para cambiar
               (cambia_valor_lista (cdr L) indices valores (add1 offset)))]))
; (cambia_valor '(1 0 1 0 0 0 1 1 1 1)
               ; (seleccion_aleatoria '(1 0 1 0 0 0 1 1 1 1)))

; (cambia_valor_lista '(1 0 1 0 0 0 1 1 1 1)
                     ; (seleccion_aleatoria '(1 0 1 0 0 0 1 1 1 1))
                     ; (list (alelo_rand)))
(define mutar
  (lambda (I) ;Individuo: I
	(cambia_valor_lista I (seleccion_aleatoria I) (list (alelo_rand)))
  )
)
; (mutar '(1 0 1 0 0 0 1 1 1 1))
;=========================COMPUTAR=========================
(define (c_2 P1)
	(list 
		P1
		(func_obj P1) 
	)
)
(define c ;C toma dos padres, los cruza, los muta y calcula su función fitness
	(lambda (padres n) 
		(list
			(c_2 (mutar (car(cdr(cruce (car (car padres)) (car (car(cdr padres))) n ) ) ) ))
			(c_2 (mutar (car(cdr(cruce (car (car padres)) (car (car(cdr padres))) n ) ) ) ))
		) 
    )
)
(define obtener_mejores_padres_gen
    (lambda (GEN)
          (list (car(car (mas_aptos (ordenar_n_grupos GEN)))) (car (car (cdr (mas_aptos (ordenar_n_grupos GEN))))))
    )
)
 ;Genera grupo en basado en el cruce y mutación de dos padres
(define (generar_grupo_hijo_t padres cant_ind resultado);padres: ((padre1,fitness padre1),(padre2, fitness padre2))
	(if (equal? 0 cant_ind)
		resultado
		(if (equal? 0 (random 0 2))
			(generar_grupo_hijo_t padres (- cant_ind 1) (cons (car (c padres (random 0 11))) resultado))
			(generar_grupo_hijo_t padres (- cant_ind 1) (cons (car (cdr (c padres (random 0 11)))) resultado))
		)
	)
)
(define (generar_grupo_hijo padres cant_ind);padres: ((padre1,fitness padre1),(padre2, fitness padre2))
	(generar_grupo_hijo_t padres cant_ind '())
)
;Genera una generación nueva apartir de la información genética de dos padres.
(define generar_gen_hija_t ;Genera una nueva generación basado en el cruce y mutación de dos padres
	(lambda (padres resultado cant_ind cant_grupos)
		(if (equal? 0 cant_grupos)
			resultado
			(if (equal? '() resultado)
			(generar_gen_hija_t padres (generar_grupo_hijo padres cant_ind) cant_ind (- cant_grupos 1))
			(generar_gen_hija_t padres (list resultado (generar_grupo_hijo padres cant_ind)) cant_ind (- cant_grupos 1))
			)
			
			
		)
	)
)
(define (generar_gen_hija padres cant_ind cant_grupos) ;Genera una nueva generación basado en el cruce y mutación de dos padres
	(generar_gen_hija_t padres '() cant_ind cant_grupos)
)


(define resolver_t
    (lambda
        (
        can_gen
        can_ind
        elit
        can_grupos
        val
        generacion_actual
        cont
        mas_apto_todo
        )
        (if (equal? can_gen cont)
            (printf "Generación ~a Individuo más apto: ~s.\n" cont mas_apto_todo)
            (begin
                (if (equal? mas_apto_todo '()); Inicialización del más apto de todo
                    (resolver_t
                    can_gen
                    can_ind
                    elit
                    can_grupos
                    val
                    generacion_actual
                    cont
                    (car(obtener_mejores_padres_gen generacion_actual))
                    )
                    (begin
                      (if (equal? (car(cdr mas_apto_todo)) 10)
                          (printf "Generación ~a Individuo más apto: ~s.\n" cont mas_apto_todo)
                          (begin
                            ;(printf "Generación ~a: ~s.\n" cont (ordenar_n_grupos generacion_actual)) 
							(printf "Generación ~a: ~s.\n" cont generacion_actual) 			
                            (printf "Los dos más aptos de la generación: ~s.\n" (obtener_mejores_padres_gen generacion_actual))
                            (resolver_t
                             can_gen
                             can_ind
                             elit
                             can_grupos
                             val
                             ;generacion_actual ;meterle la nueva generación
							 (generar_gen_hija (obtener_mejores_padres_gen generacion_actual) can_ind can_grupos)
                             (+ cont 1)
                             (individuo_mas_apto mas_apto_todo (car (obtener_mejores_padres_gen generacion_actual)))
                             )
                           )
                       )
                    )
                )
            )
        )
    )
)

(define resolver
	(lambda (can_gen can_ind elit can_grupos val)
		(resolver_t
                 can_gen can_ind #t can_grupos '() (ordenar_n_grupos(generar_n_grupos can_ind #t can_grupos)) 0 '()
        )
	)
)
(define (custom_read msg)
	(printf msg)
	(read)
)
(define (invocar_a_resolver)
	(resolver 
	(custom_read "Digite la cantidad de generaciones: ")
	(custom_read "Digite la cantidad de individuos: ") 
	#t
	(custom_read "Digite la cantidad de grupos: ") 
	'()
	)
)

(define (menu)
	(begin
		(invocar_a_resolver)
		(menu)
	)
)
(menu)