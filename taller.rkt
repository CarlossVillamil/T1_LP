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







;;Pregunta 7 Elabore una funci´on llamada cartesian-product que recibe como
;;argumentos 2 listas de s´ımbolos sin repeticiones L1 y L2. La funci´on debe
;;retornar una lista de tuplas que representen el producto cartesiano entre L1
;;y L2. Los pares pueden aparecer en cualquier orden. 


;; Función principal: cartesian-product
;; Recibe dos listas L1 y L2 y retorna su producto cartesiano.
(define cartesian-product
  (lambda (L1 L2)

    
    (cond
      [(null? L1) empty]

      [else
       (juntarListas (juntar-elementos (car L1) L2)
               (cartesian-product (cdr L1) L2))])))




;; Función auxiliar: juntar-elementos
;; Toma un elemento 'x' y lo combina con cada elemento de una lista L2.
;; Ejemplo: (pair-element 'a '(x y)) -> ((a x) (a y)) donde a sera el Car de la lista L1
(define juntar-elementos
  (lambda (x L2)
    (cond
     [(null? L2) empty]

     [else
        (cons (list x (car L2))
                (juntar-elementos x (cdr L2)))])))



;; Función auxiliar:juntarListas para no usar append 
(define juntarListas
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1)
              (juntarListas (cdr L1) L2) ) )

  )
       )
             

;;(cartesian-product '(a b c) '(x y 9))



;;Pregunta 8 Elabore una funci´on llamada mapping que debe recibir como
;;entrada 3 argumentos: una funci´on unaria (que recibe un argumento) llamada
;;F, y dos listas de n´umeros L1 y L2. La funci´on debe retornar una lista de
;;pares (a,b) siendo a elemento de L1 y b elemento de L2, cumpli´endose la
;;propiedad que al aplicar la funci´on unaria F con el argumento a, debe arrojar
;;el n´umero b. Es decir, se debe cumplir que F(a) = b. (Las listas deben ser
;;de igual tama˜no)


;; Función principal: mapping
;; x: función unaria, L1 y L2: listas de números
(define mapping
  (lambda (x L1 L2)
    (cond
      ;;revisamos si las listas no estan vacias 
      [(null? L1) empty]
      
      ;; Si x aplicado al primer elemento de L1 es igual al primero de L2
      ;; usamos eqv? para saber si el primer elemento de la lista L1, y aplicado la funcion
      ;; es igual a el primero elemento de la lista L2, si es asi contruimos la lista y seguimos
      ;; con los siguientes dos elementos.
      ((eqv? (x (car L1)) (car L2))
       (cons (list (car L1) (car L2)) 
             (mapping x (cdr L1) (cdr L2))))
      
      ;; Si no se cumple la condición,volvemos a llamar la funcion  con los siguientes dos elemntos 
      (else (mapping x (cdr L1) (cdr L2))))))


;;(mapping (lambda (x) (* x 2)) '(1 2 3) '(2 4 6)) 
