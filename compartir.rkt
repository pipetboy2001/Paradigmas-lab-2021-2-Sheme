#lang racket
(provide (all-defined-out))
(require "Registro.rkt")

;finalizar y mandar a otro doc
;(char<? #\r)
;(char<? #\w)
;(char<? #\c)

(define (endParadoc end-SesionError otherDoc)
  (list end-SesionError otherDoc)
  )
;acceso
(define (access user accion)
  (if accion = "r") ; posible funcion string?
  Paradoc->users accion
  (if accion = "w")
  Paradoc->users accion ;podria meter funcion create?
  (if accion = "c")
  Paradoc->users user ;posible funcion comment a futuro?
  )