#lang racket
(provide (all-defined-out))
(require "Create.rkt")
(require "compartir.rkt")

;empezar una busqueda
(define (buscandolo  buscando doc)
  (occurrence buscando doc);si se repire 
        (member buscando doc) ;ve el elemento 
           (endParadoc (endSesion) (cdr doc)));finalizar)


;cuantas veces ocurre
(define (occurrence x lst)
       (if (null? lst) 0    
            (if (equal? x (car lst))  (+ 1 (occurrence x (cdr lst)))
                                     (occurrence x (cdr lst)) 
            ))) 


;true or false si se encuentra 
(define (member? x list)
     (cond ((null? list) #f)
           ((equal? x (car list)) #t)
           (else   (member? x (cdr list)))))