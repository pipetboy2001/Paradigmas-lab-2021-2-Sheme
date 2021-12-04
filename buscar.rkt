#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
;(require "compartir.rkt")

;---------------
;Representacion
;_________--____
;EN ESTE CASO BUSCAR EL HOLA
;Hola en paradoc buscar...
;( "Pipe" "123asd" search)Paradoc ) "hola"))
;_-------------
;Constructor
;---------------
;empezar una busqueda
(define (buscandolo  buscando doc)
  (occurrence buscando doc);si se repire 
        (member buscando doc) ;ve el elemento 
           (endParadoc (endSesion) (cdr doc)));finalizar)

;---------------
;PERTENENCIA
;---------------
;true or false si se encuentra 
(define (member? x list)
     (cond ((null? list) #f)
           ((equal? x (car list)) #t)
           (else   (member? x (cdr list)))))

;cuantas veces ocurre
(define (occurrence x lst)
       (if (null? lst) 0    
            (if (equal? x (car lst))  (+ 1 (occurrence x (cdr lst)))
                                     (occurrence x (cdr lst)) 
            ))) 