#lang racket
(provide (all-defined-out))

; TDA Añadir

;---------------------------------------------------
; FUNCION REPRESENTACION
;---------------------------------------------------
;(newAdds idAdd autor permisos fecha estado reportes votos contenido)
;ejemplo:(int date string string list)

; EJEMPLOS:
; '() doc de respuesta vacio
; '(1 "Genesis" ("anime" "games") (11 09 09) #f 0 (0 0) "El mejor juego de todos es Life is Strange")
; '(((6 "Naomi" ("anime" "games") (16 11 2020) #f 0 (0 0) "El mejor juego es God of War")
;    (2 "Levy" ("series" "ejercicio") (08 07 13) #f 0 (0 0) "Debes hacer varias repeticiones con peso y comer mucho")).

;---------------------------------------------------
; FUNCION CONSTRUCTOR
;---------------------------------------------------
; Función que permite crear una respuesta y la agrega a Stack.
; Dominio: doc answers.
; Recorrido: stack.
(define (newAddDoc adds)
  (list adds)
  )

;---------------------------------------------------
; FUNCION PERTENENCIA
;---------------------------------------------------
; Función que determina si una respuesta cumple con requisitos para ser una respuesta.
; Dominio: añadir(list)
; Recorrido: boolean.
(define (Add? añadir)
  (if (null? añadir)
      #t
      (if (and (list? añadir) (= 8 (length añadir)))
               (if (and
                    (integer? (add->id añadir))
                    (string? (add->author añadir))
                    (list? (add->permisos añadir))
                    (and (list? (add->date añadir)) (= 3 (length (add->date añadir))))
                    (boolean? (add->state añadir))
                    (integer? (add->report añadir))
                    (and (list? (add->votes añadir)) (= 2 (length (add->votes añadir))))
                    (string? (add->content añadir))
                    )
                   #t
                   #f
                   )
          #f
       )
   )
 )

; Función que determina si una respuesta es válida en el stack.
; Dominio: stack.
; Recorrido: boolean.
(define (Adds? adds)
  (if (null? adds)
      #t
      (if (Add? (car adds))
          (Adds? (cdr adds))
          #f
      )
  )
)

;---------------------------------------------------
; FUNCIONES SELECTORAS
;---------------------------------------------------
; Función que retorna el id de una respuesta.
; Dominio: añadir(list)
; Recorrido: number
(define (add->id añadir)
  (car añadir)
  )

; Función que retorna el autor de una respuesta.
; Dominio: añadir(list)
; Recorrido: string
(define (add->author añadir)
  (cadr añadir)
  )

; Función que retorna las etiquetas de una respuesta.
; Dominio: añadir(list)
; Recorrido: list
(define (add->permisos añadir)
  (caddr añadir)
  )

; Función que retorna la fecha de una respuesta.
; Dominio: añadir(list)
; Recorrido: date(list)
(define (add->date añadir)
  (cadddr añadir)
  )

; Función que retorna el estado de una respuesta.
; Dominio: añadir(list)
; Recorrido: boolean
(define (add->state añadir)
  (cadr (cdddr añadir))
  )

; Función que retorna los reportes de una respuesta.
; Dominio: añadir(list)
; Recorrido: number
(define (add->report añadir)
  (caddr (cdddr añadir))
  )

; Función que retorna la lista de votos de una respuesta.
; Dominio: añadir(list)
; Recorrido: list
(define (add->votes añadir)
  (cadddr (cdddr añadir))
  )

; Función que retorna el contenido de una respuesta.
; Dominio: añadir(list)
; Recorrido: string
(define (add->content añadir)
  (cadr (cdddr (cdddr añadir)))
  )

; Función que retorna el número de respuestas en el stack.
; Dominio: stackAnswer
; Recorrido: añadir(list)
(define (docAdd->AddsQ doc)
  (car doc)
  )