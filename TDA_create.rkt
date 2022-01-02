#lang racket
(provide (all-defined-out))
; TDA Creacion

;---------------------------------------------------
; FUNCION REPRESENTACION
;---------------------------------------------------
; (newDoc id author votes views permisos createDate lastActivity state reward numAdds content)

; EJEMPLOS:
; '()  de pregunta vacío
; '(1 "nagito" (0 0) 0 ("tagAnime" "tagCyber" "tagPunk") ((11 09 19) (01 01 21)) "OPEN" 0 0 "por qué es azul el cielo")
; '((1 "nagito" (0 0) 0 ("tagAnime" "tagCyber" "tagPunk") ((11 09 19) (01 01 21)) "OPEN" 0 0 "por qué es azul el cielo"))
;   (2 "usagi" (0 0) 0 ("salud" "mente" "depresion") ((24 12 18) (11 10 19)) "CLOSE" 0 0 "Por qué estoy deprimido"))
;---------------------------------------------------
; FUNCION CONSTRUCTOR
;---------------------------------------------------
;---------------------------------------------------
; FUNCION PERTENENCIA
;---------------------------------------------------
; Función que comprueba si una pregunta cumple para ser una pregunta.
; Dominio: list.
; Recorrido: boolean.
(define (create? create)
  (if (list? create)
      (if  (null? create)
           #t
           (if (and (= (length create) 10)
                    (number? (create->id create))
                    (string? (create->author create))
                    (and (list? (create->votes create)) (= 2 (length (create->votes create)))
                         (and (number? (votes->a (create->votes create))) (number? (votes->a (create->votes create)))))
                    (number? (create->views create))
                    (permiso? (create->permisos create))
                    (and (date? (date->dateDoc (create->Date create))) (date? (date->dateLast (create->Date create))))
                    (or (equal? "OPEN" (create->state create)) (equal? "CLOSE" (create->state create)))
                    (number? (create->reward create))
                    (number? (create->numAdd create))
                    (string? (create->content create))
                    )
                    #t
                    #f
            )
      )
      #f
    )
  )

; Función que comprueba si las etiquetas ingresadas son correctas.
; Dominio: list.
; Recorrido: boolean.
(define (permiso? permisos)
  (if (null? permisos) 
      #t
      (if (string? (car permisos))
          (permiso? (cdr permisos))
          #f
       )
  )
 )
;---------------------------------------------------
; FUNCIONES SELECTORAS
;---------------------------------------------------
; Función que retorna el id de la pregunta.
; Dominio: create(list).
; Recorrido: number.
(define (create->id create)
  (car create)
 )
; Función que retorna el autor de la pregunta.
; Dominio: create(list).
; Recorrido: string.
(define (create->author create)
  (cadr create)
 )
; Función que retorna una lista con los votos de la pregunta.
; Dominio: create(list).
; Recorrido: list.
(define (create->votes create)
  (caddr create)
 )
; Función que retorna las visualizaciones de la pregunta.
; Dominio: create(list).
; Recorrido: number.
(define (create->views create)
  (cadddr create)
 )
; Función que retorna las etiquetas de la pregunta.
; Dominio: create(list).
; Recorrido: list.
(define (create->permisos create)
  (cadr (cdddr create))
 )
; Función que retorna la fecha y edición de la pregunta.
; Dominio: create(list).
; Recorrido: list.
(define (create->Date create)
  (caddr (cdddr create))
 )
; Función que retorna el estado de la pregunta.
; Dominio: create(list).
; Recorrido: string.
(define (create->state create)
  (cadddr (cdddr create))
 )
; Función que retorna la recompensa de la pregunta.
; Dominio: create(list).
; Recorrido: number.
(define (create->reward create)
  (cadr (cdddr (cdddr create)))
 )
; Función que retorna las respuestas de la pregunta.
; Dominio: create(list).
; Recorrido: number.
(define (create->numAdd create)
  (caddr (cdddr (cdddr create)))
 )
; Función que retorna el contenido de la pregunta.
; Dominio: create(list).
; Recorrido: string.
(define (create->content create)
  (cadddr (cdddr (cdddr create)))
  )
;---------------------------------------------------
; OTRAS FUNCIONES
;---------------------------------------------------
; Función que retorna los votos positivos.
; Dominio: list.
; Recorrido: number. 
(define (votes->a votes)
  (car votes)
  )
; Función que selecciona la fecha de publicación.
; Dominio: list.
; Recorrido: list.
(define (date->dateDoc date)
  (car date)
  )
; Función que selecciona la fecha de actualización.
; Dominio: list.
; Recorrido: list. 
(define (date->dateLast date)
  (cadr date)
  )
