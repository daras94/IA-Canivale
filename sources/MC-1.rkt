#lang racket
(require dyoo-while-loop)

(define (quick-sort l)
  (cond
     [(empty? l) empty]
     [else (append (quick-sort (menor-que (car l) (cdr l)))         ; filtramos y ordenamos aquellos elentos menores que el pivote.
                   (list (first l))                                 ; Elemento de pivote
                   (quick-sort (mayor-igual-que (car l) (cdr l))))  ; filtramos y ordenamos aquellos elentos menores que el pivote.
     ]
  )
)
(define (menor-que x l) (filter (lambda (y) (< (coste-camino y) (coste-camino x))) l))
(define (mayor-igual-que x l) (filter (lambda (y) (>= (coste-camino y) (coste-camino x))) l))

(define (suscesores l) )


(define (CanibalesYMisionero)
  (let* ( ; Declaracion de variables.
         (actual    '())              ; Lista de nodos actuales el camino designado.
         (sucesores '())              ; Lista de sucesores
         (abierto   '('(0 0 1 3 3)))  ; Lista de abiertos (My, Cy, B, Mx, Cx).
         (meta      '(3 3 0 0 0))
        ) 
    (while (not (eq? meta (car actual)))  ; Bucle de busqueda en el arbol.
        (cond (car abierto) actual)       ; Estraemos el elmento de la lista de actuales
        (cdr abierto)
        (if (eq? meta (car actual))
            (printf "Numero de moviento: ~s" (length actual))
               
        )
    )
  )
)
