#lang racket
(provide (all-defined-out))
;FECHAS

;Constructor
; Función que crea una lista de fecha.
; Dominio: number x number x number. 
; Recorrido: list.
;DEFINO date dias mes año
(define (date dd mm aa)
  (list dd mm aa)
  )

;Selector
;dia 
(define (date->day date)
  (car date)
  )
;mes
(define (date->month date)
  (cadr date)
  )
;año
(define (date->year date)
  (caddr date)
  )