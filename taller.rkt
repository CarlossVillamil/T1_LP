#lang eopl

;PUNTO 1

;; invert :
;; Proposito:
;; (List (List X X)) x (X -> Predicado) -> (List (List X X))
;; Procedimiento que recibe una lista de pares L y un predicado P.
;; Retorna una lista donde cada par (x y) se convierte en (y x)
;; únicamente si tanto x como y cumplen el predicado P.
;;
;; <lista> := ()
;;          := (<par> <lista>)
;;
;; <par> := (<valor> <valor>)
;;
;; <valor> := <valor-de-scheme>

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
(invert '((1 3) (2 4) (6 8)) even?)



;PUNTO 2

;; down :
;; Proposito:
;; List -> List
;; Procedimiento que recibe una lista L y retorna una nueva lista
;; donde cada elemento x ha sido transformado en una lista (x).
;; Aumenta en uno el nivel de anidamiento.
;;
;; <lista> := ()
;;          := (<valor> <lista>)
;;
;; <valor> := <valor-de-scheme>

;; envolver :
;; Proposito:
;; X -> (List X)
;; Recibe un elemento x y lo envuelve en una lista.
;;
;; <lista-unitaria> := (<valor>)

(define envolver
  (lambda (x)
    (cons x empty)))

(define down
  (lambda (L)
    (if (null? L)
        empty
        (cons
         (envolver (car L))
         (down (cdr L))))))

;;Pruebas
(down '(3 2 4))
(down '( (UNA) (BUENA) (IDEA) ))
(down '(un (objeto (mas)) complicado))


;PUNTO 3

;; list-set :
;; Proposito:
;; List x Number x X x (X -> Predicado) -> List
;;
;; Procedimiento que reemplaza el elemento en la posición n
;; de la lista L por el valor x, únicamente si dicho elemento
;; cumple el predicado P.
;;
;; Si el elemento en la posición n no cumple el predicado,
;; la lista se retorna sin modificación.
;;
;; <lista> := ()
;;          := (<valor> <lista>)
;;
;; <valor> := <valor-de-scheme>

(define list-set
  (lambda (L n x P)
    (cond
      [(null? L) empty]
      [(= n 0) (if (P (car L)) (cons x (cdr L)) L)]
      [(> n 0) (cons (car L) (list-set (cdr L) (- n 1) x P))]

      )

    )

  )

;;PRUEBAS
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)



;;FALTA COMENTAR
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



;;FALTA COMENTAR
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



;; juntarListas :
;; Proposito:
;; List x List -> List
;;
;; Procedimiento que recibe dos listas L1 y L2 y retorna
;; una nueva lista que contiene todos los elementos de L1
;; seguidos de los elementos de L2.
;;
;; Implementa manualmente el comportamiento de append,
;; Si L1 es vacía, retorna L2.
;;
;; <lista> ::= ()
;;           ::= (<valor> <lista>)
;;
;; <valor> ::= <valor-de-scheme>

(define juntarListas
  (lambda (L1 L2)
    (if (null? L1)
        L2
        
        (cons (car L1)
              (juntarListas (cdr L1) L2)))))
             

;;(cartesian-product '(a b c) '(x y 9))


;;FALTA COMENTAR
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


;;PUNTO 11

;; zip :
;; Proposito:
;; (X x Y -> Z) x (List X) x (List Y) -> (List Z)
;;
;; Procedimiento que recibe una función binaria F y dos listas
;; L1 y L2 de igual tamaño. Retorna una nueva lista donde
;; la posición n-ésima corresponde al resultado de aplicar F
;; a los elementos en la posición n-ésima de L1 y L2.
;;
;; <lista> := ()
;;          := (<valor> <lista>)
;;
;; <valor> := <valor-de-scheme>
;;
;; Precondición:
;; L1 y L2 deben tener la misma longitud.

(define zip
  (lambda (F L1 L2)
    (cond
      [(and (null? L1) (null? L2) empty)]
      [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]

      )
  )
)

;;PRUEBAS
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))


;;PUNTO 15

;; count-odd-and-even :
;; Proposito:
;; Arbol-Binario -> (List Number Number)
;;
;; Procedimiento que recibe un árbol binario representado como
;; una lista de la forma (valor subarbol-izq subarbol-der),
;; donde valor es un número.
;;
;; Retorna una lista de dos elementos:
;; (num-pares num-impares)
;;
;; - num-pares   : cantidad total de números pares en el árbol.
;; - num-impares : cantidad total de números impares en el árbol.
;;
;; Si el árbol es vacío '(), retorna (0 0).
;;
;; <arbol> ::= ()
;;           ::= (<numero> <arbol> <arbol>)
;;
;; <numero> ::= <number-de-scheme>
;;
;; <resultado> ::= (<numero> <numero>)


(define count-odd-and-even
  (lambda (arbol)
    (if (null? arbol)
        
        (cons 0 (cons 0 empty))
        
        
        (cons
         
         (+ (if (even? (car arbol)) 1 0)
            (car (count-odd-and-even (cadr arbol)))
            (car (count-odd-and-even (caddr arbol))))
         
         
         (cons
          (+ (if (odd? (car arbol)) 1 0)
             (cadr (count-odd-and-even (cadr arbol)))
             (cadr (count-odd-and-even (caddr arbol))))
          '())))))

;;PRUEBAS
(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
(count-odd-and-even '(5 (2 () ()) (8 () ())))
(count-odd-and-even '())

;;PUNTO 16


;; hanoi :
;; Proposito:
;; Number x Simbolo x Simbolo x Simbolo -> (List (List Simbolo Simbolo))
;;
;; Procedimiento que recibe:
;;   n        : número de discos
;;   origen   : torre origen
;;   auxiliar : torre auxiliar
;;   destino  : torre destino
;;
;; Retorna una lista de movimientos necesarios para resolver
;; el problema de las Torres de Hanoi.
;;
;; Cada movimiento se representa como una lista:
;;   (origen destino)
;;
;; Si n = 1, retorna una lista con un único movimiento.
;;
;; <movimientos> ::= ()
;;                ::= (<movimiento> <movimientos>)
;;
;; <movimiento> ::= (<simbolo> <simbolo>)
;;
;; <simbolo> ::= <symbol-de-scheme>

(define hanoi
  (lambda (n origen auxiliar destino)
    (if (= n 1)

        ;; Caso base: un solo movimiento
        (cons
          (cons origen
                (cons destino '()))
          '())

        ;; Caso recursivo
        (juntarListas
          ;; 1. Mover n-1 discos a la torre auxiliar
          (hanoi (- n 1)
                 origen
                 destino
                 auxiliar)

          (juntarListas
            ;; 2. Mover el disco mayor al destino
            (cons
              (cons origen
                    (cons destino '()))
              '())

            ;; 3. Mover los n-1 discos al destino
            (hanoi (- n 1)
                   auxiliar
                   origen
                   destino))))))