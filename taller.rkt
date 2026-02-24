#lang eopl

;PUNTO 1

; invert : (List (List X X)) x (X -> Predicado) -> (List (List  X X))
; usage: (invert L P) = retorna una lista donde los pares (x y) se convierten en (y x) unicamente si tanto x como y cumplen el predicado P. 

(define invert
  (lambda (L P)
    (cond
      [(null? L) empty]

      [(and (P (caar L)) (P (cadar L)))
       (cons (list (cadar L) (caar L))
             (invert (cdr L) P))]

      [else
       (invert (cdr L) P)])))


;;Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)


;PUNTO 2

; down : List -> List
; usage: (down L) = retorna una lista donde cada elemento x de L 
;                  ha sido transformado en una lista (x)
;                  Aumenta en uno el nivel de anidamiento de cada elemento

(define down
  (lambda (L)
    (if (null? L)
        empty ; Caso base: lista vacía
        (cons
         (envolver (car L)) ; Procesa el primer elemento
         (down (cdr L)))    ; Llamada recursiva con el resto de la lista
        )
    )
  )

; envolver : X -> (List X)
; usage: (envolver x) = envuelve el elemento x dentro de una lista

(define envolver
  (lambda (x)
    (cons x empty)
    )
  )

;;Pruebas
(down '(3 2 4))
(down '( (UNA) (BUENA) (IDEA) ))
(down '(un (objeto (mas)) complicado))