#lang racket
(require dyoo-while-loop)
(define (CanibalesYMisionero g)
  (let* ( ; Declaracion de variables.
         (actual    '())              ; Lista de nodos actuales el camino designado.
         (sucesores '())              ; Lista de sucesores
         (abierto   '('(0 0 1 3 3)))  ; Lista de abiertos (My, Cy, B, Mx, Cx).
         (meta      '(3 3 0 0 0))
        ) 
        (while (not (eq?  meta (car abierto)))
           (printf "hola")
        )
  )
)