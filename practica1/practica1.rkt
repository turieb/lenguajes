#lang plai

;; Nos permite saber si un numero es Natural
;; natural?:number->bool
(define natural?
  (lambda (x)
    (if (and (>= x 0) (integer? x))
        #t
        #f
        )))

;;Funcion ackerman 
;;ackerman:number, number -> number
(define ackerman 
  (lambda (x y)
    (if (and (natural? x) (natural? y))
    (cond
      [(= x 0) (+ y 1)]
      [(and (> x 0) (= y 0)) (ackerman (- x 1) 1)]
      [(and (> x 0) (> y 0)) (ackerman (- x 1) (ackerman x (- y 1)))]
    [else "Error los numeros no son matematicos"])
    "Error, los numeros no son naturales")
    ))

;;Filtra el contenido de una lista
;;filtro: listof(A), predicado -> listof(predicado)
(define filtro
  (lambda (lista f)
    (if (empty? lista)
        empty
        (if (f (car lista))
            (cons (car lista) (filtro (cdr lista) f ))
            (filtro (cdr lista) f)
            )
        )))

;;Funcion auxiliar para derivadas
(define derivada
  (lambda (list)
    (cond
      [(= 1 (length list)) '()]
      [else (let ([e (- (length list) 1)]) 
                  (cons (* (car list) e) (derivada (cdr list)))
                  )]
      )
    ))

;; Funcion que multiplica los elementos de la lista por un numero
(define multi-lista
  (lambda (list x)
    (if (empty? list) 
        empty
        (cons (* (car list) (expt x (- (length list) 1))) (multi-lista (cdr list) x) ))
    ))

;;Suma listas pero ya debes de estar volteadas
(define suma-polinomios
  (lambda (l1 l2)
    (cond 
      [(empty? l1) l2]
      [(empty? l2) l1]
      [else (cons (+ (car l1) (car l2)) (suma-polinomios (cdr l1) (cdr l2)))]
      )
    ))

;;Funcion que deriva, suma y evalua dos polinomios
;;deriva-suma-dificil:list, list, number -> list
(define deriva-suma-dificil
  (lambda (l1 l2 v) 
    (foldl + 0 (multi-lista (reverse (suma-polinomios (reverse (derivada l1)) (reverse (derivada l2)))) v)) 
    ))

;;Funcion que deriva, suma y evalua dos polinomios 
;;deriva-suma:list, list, number -> list
(define deriva-suma
  (lambda (poli1 poli2 valor)
    (let* ([derivada1 (derivada poli1)]
           [derivada2 (derivada poli2)]
           [sum (suma-polinomios (reverse derivada1) (reverse derivada2))]
           [susti (multi-lista (reverse sum) valor)])
      (foldl + 0 susti)
      )))

;;Dada una lista l de longitud par y una funcion f de aridad 2, construye
;;una lista con el resultado de aplicar la funcion al primero elemento
;;y al ultimo hasta quedar vacia
;;primero-y-ultimo:(l f) -> l
(define primero-y-ultimo
  (lambda (list f)
    (cond 
      [(= (modulo (length list) 2) 0) 
       (if (empty? list) 
           empty
           (cons (f (car list) (car (reverse list))) (primero-y-ultimo (cdr (reverse (cdr list))) f )))
       ]
      [else "Error la lista debe de ser de tamano par"]
      )))


;;Nos permite conocer dadas dos listas  cual es mayor
;;compara-longitud-listas:list,list->String
(define compara-longitud-listas
  (lambda (l1 l2)
    (cond
      [(and (empty? l1) (empty? l2)) "listas-iguales"]
      [(empty? l1) "lista2-mas-grande"]
      [(empty? l2) "lista1-mas-grande"]
      [else
       (compara-longitud-listas (cdr l1) (cdr l2) )
       ]
      )))

;;entierra el simbolo un numero determinado de veces
;;entierra:symbol, number-> list
(define entierra
  (lambda (s n)
    (cond 
      [(= n 0) s]
      [(= n 1) (cons s '())]
      [else (cons (entierra s (- n 1)) '())]
      )
    ))

;;Construye una lista con los elementos de las posiciones nones de una lista
;;uno-si-uno-no:list->list
(define uno-si-uno-no
  (lambda (lista)
    (cond
     [(empty? lista) empty]
     [(empty? (cdr lista)) (cons (car lista) '())]
     [else (cons (car lista) (uno-si-uno-no (cddr lista)))]
     )))

;;Dadas dos listas ordenadas crea una nueva mezclandolas y manteniendo el orden
;;merge:list, list->list
(define merge
  (lambda (l1 l2)
    (cond
      [(and (empty? l1) (empty? l2)) empty]
      [(empty? l1) l2]
      [(empty? l2) l1]
      [else
       (cond 
         [(<= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2 ))]
         [else (cons (car l2) (merge l1 (cdr l2))) ]
        )
       ]
      )))


;;Ordena una lista de listas dadas su longitud
;;ordena-por-longitud:listaof(list)
(define ordena-por-longitud
  (lambda (lista)
    (letrec ([menor-longitud (lambda (lista1 lista2)
                               (if (< (length lista1) (length lista2) ) #t #f)
                               )])
    (sort lista menor-longitud)
    )
    ))
