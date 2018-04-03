#lang racket

(define (suscesores num_op nodo_actual)
  (let* (;Declaramos las variables para las 5 operaciones posibles estraidas.
         (op_transicion (list (list 0 1 1 0 -1) (list 1 0 1 -1 0) (list 2 0 1 -2 0) (list 0 2 1 0 -2) (list 1 1 1 -1 -1)))
         (new_sucesor   '())
         (cp_nodo_actual nodo_actual)
        )
    ;Declaramos las expresiones a usar.
    (cond
      [(< num_op 5)
        (set! new_sucesor (map (lambda (l1 l2) (+ l1 l2)) (list-ref op_transicion num_op) nodo_actual))
        (printf "Beta Sucesores: ~v \n" new_sucesor)
        (if (and (>= (list-ref new_sucesor 4) 0) (>= (list-ref new_sucesor 3) 0) (eq? (apply + new_sucesor) 7) (>= (list-ref new_sucesor 3) (list-ref new_sucesor 4))) ; validamos que Mx >= Cx en la orilla de partida.
            (append (suscesores (+ num_op 1) nodo_actual) (cons new_sucesor '()))
            (suscesores (+ num_op 1) nodo_actual)
        )]
      [else empty] ; Finaliza la construcion de los sucesores.
    )
  )
)

; Calculamos su coste para su ordenacion.
(define (fun n coste)
  (reverse (cons (+ (- (* 2 (+ (list-ref n 3) (list-ref n 4))) (list-ref n 2)) coste) (list n)))
)

(require dyoo-while-loop)
(define (MC1)
  (let* ( ; Declaracion de variables.
         (actual    (list null)                      )  ; Lista de nodos actuales el camino designado.
         (sucesor   (list null)                      )  ; Lista de sucesores
         (abiertos  (list (list (list 0 0 1 3 3) 0)) )  ; Lista de abiertos (My, Cy, B, Mx, Cx).
         (meta      (list 3 3 0 0 0)                 )  ; Definimos el estao meta del problema.
        ) 
    (while (and (not (equal? meta (car actual))) (not (empty? abiertos)))  ; Bucle de busqueda en el arbol.
        (set! actual   (append (list (map (lambda (l1 l2) (- l1 l2)) (caar abiertos) (list 0 0 1 0 0))) (remove '() actual))) ; Estraemos el elmento de la lista de actuales l.
        (cond
          [(equal? meta (car actual))
              (println "\n/*                     -----> Solucion MC1: <-----                    */")
              (println "/**********************************************************************/")
              (printf " - Numero de moviento: ~s \n - Camino seguido: ~s" (length actual) (reverse actual))
          ]
          [else
             (printf " Actuales: ~v \n" actual)
             (printf "\n/*         -----> Generandos Sucesores de ~v: <-----         */ \n" (car actual))
             (println "/**********************************************************************/")
             (set! sucesor  (map (lambda (s1) (fun s1 (length actual))) (suscesores 0 (car actual))))
             (printf " Sucesores: ~v \n" sucesor)
             (set! abiertos (sort (append (cdr abiertos) sucesor) #:key last <))
             (printf " Abiertos: ~v \n" abiertos)
          ]
        )
    )
  )
)