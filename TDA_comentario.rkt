#lang racket
(provide (all-defined-out))

(define getUsername car)
(define getRegistro3 caddr)
(define getCreate3 car)
(define getUsuarioActivo3(lambda(doc)
                           (car (cadr doc))))
(define getRegistro cadddr)
(define getComentario car)
(define getCreate cadr)
(define getUsuarioActivo(lambda(doc)
                           (car (caddr doc))))
