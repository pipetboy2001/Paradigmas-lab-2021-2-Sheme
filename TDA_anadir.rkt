#lang racket
(provide (all-defined-out))

; TDA Añadir

;---------------------------------------------------
; FUNCION REPRESENTACION
;---------------------------------------------------
;(newAdds idAdd autor permisos fecha estado contenido)
;ejemplo:(int date string string list)

; EJEMPLOS:
; '() doc de respuesta vacio
; '(1 "Genesis" 'w' (11 09 09) #f 0 (0 0) "El mejor juego de todos es Life is Strange")
; '(((6 "Naomi"'c' (16 11 2020) #f 0 (0 0) "El mejor juego es God of War")
;    (2 "Levy" 'r' (08 07 13) #f 0 (0 0) "Debes hacer varias repeticiones con peso y comer mucho")).

;---------------------------------------------------
; FUNCION CONSTRUCTOR
;---------------------------------------------------
; Función que permite crear una respuesta y la agrega a Stack.
; Dominio: doc add.
; Recorrido: doc.
(define (newAddDoc adds)
  (list adds)
  )
; Función que crea una respuesta.
; Dominio: number x string x list x number x string x number x list x string
; Recorrido: stack.
(define (newAdds idAdd authorUser permisos dateAnswer acceptedState reports votes answerContent)
  (list idAdd authorUser permisos dateAnswer acceptedState reports votes answerContent)
  )

;---------------------------------------------------
; FUNCION PERTENENCIA
;---------------------------------------------------
; Función que determina si una respuesta cumple con requisitos para ser un add.
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
                    (and (list? (add->listi añadir)) (= 2 (length (add->listi añadir))))
                    (string? (add->content añadir))
                    )
                   #t
                   #f
                   )
          #f
       )
   )
 )

; Función que determina si una respuesta es válida en el doc.
; Dominio: doc.
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
; Función que retorna el id de un añadido.
; Dominio: añadir(list)
; Recorrido: number
(define (add->id añadir)
  (car añadir)
  )

; Función que retorna el autor del añadido.
; Dominio: añadir(list)
; Recorrido: string
(define (add->author añadir)
  (cadr añadir)
  )

; Función que retorna los permisos.
; Dominio: añadir(list)
; Recorrido: list
(define (add->permisos añadir)
  (caddr añadir)
  )

; Función que retorna la fecha de unañadido.
; Dominio: añadir(list)
; Recorrido: date(list)
(define (add->date añadir)
  (cadddr añadir)
  )

; Función que retorna el estado de unañanadido.
; Dominio: añadir(list)
; Recorrido: boolean
(define (add->state añadir)
  (cadr (cdddr añadir))
  )

; Función que retorna los reportes de un añadido.
; Dominio: añadir(list)
; Recorrido: number
(define (add->report añadir)
  (caddr (cdddr añadir))
  )

; Función que retorna la lista.
; Dominio: añadir(list)
; Recorrido: list
(define (add->listi añadir)
  (cadddr (cdddr añadir))
  )

; Función que retorna el contenido .
; Dominio: añadir(list)
; Recorrido: string
(define (add->content añadir)
  (cadr (cdddr (cdddr añadir)))
  )

; Función que retorna el número de añadidos.
; Dominio: stackAnswer
; Recorrido: añadir(list)
(define (docAdd->AddsQ doc)
  (car doc)
  )