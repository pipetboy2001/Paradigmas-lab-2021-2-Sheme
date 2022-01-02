#lang racket
(provide (all-defined-out))
(require "TDA_usuario.rkt")
(require "TDA_create.rkt")
(require "TDA_anadir.rkt")

;TDA AUXILIAR

;---------------------------------------------------
; FUNCION REPRESENTACION
;---------------------------------------------------
; '(() () () ()) doc vacio.
; '(users create answers) doc con elementos correspondientes.

;---------------------------------------------------
; FUNCION CONSTRUCTOR
;---------------------------------------------------
; Función que crea un doc inicial vacío.
; Dominio: 
; Recorrido: doc vacío.
(define (GoogleDoc)
  '(() () () ())
  )

; Función que comprueba si los nuevos doc son válidos.
; Dominio: stack.
; Recorrido: boolean.
(define (GoogleDoc? GoogleDoc)
  (if (list? GoogleDoc)
      (if (= 4 (length GoogleDoc))
          (if (and (user? (GoogleDoc->loginActual GoogleDoc))
                   (validDoc user? (GoogleDoc->users GoogleDoc))
                   (validDoc create? (GoogleDoc->Create GoogleDoc))
                   (validDoc Adds? (GoogleDoc->Add GoogleDoc))
               )
              #t
              #f
              )
          #f
       )
      #f
   )
  )
; Función que comprueba si el doc es válido.
; Dominio: stack.
; Recorrido: boolean.
(define (validDoc validation doc2)
  (if (null? doc2)
      #t
      (if (validation (car doc2))
          (validDoc validation (cdr doc2))
          #f
          )
      )
  )
;---------------------------------------------------
; FUNCIONES SELECTORAS
;---------------------------------------------------

; Función que retorna los usuarios conectados.
; Dominio: stack.
; Recorrido: doc(list).
(define (GoogleDoc->users GoogleDoc)
  (cadr GoogleDoc)
  )
; Función que retorna el usuario que ha iniciado sesión.
; Dominio: stack.
; Recorrido: user(list)
(define (GoogleDoc->loginActual GoogleDoc)
  (car GoogleDoc)
  )
; Función que retorna las preguntas de los usuarios.
; Dominio: stack.
; Recorrido: stackQuestions.
(define (GoogleDoc->Create GoogleDoc)
  (caddr GoogleDoc)
  )
; Función que retorna las respuestas a las preguntas de los usuarios.
; Dominio: doc
; Recorrido: doc de doc de Respuestas
(define (GoogleDoc->Add GoogleDoc)
  (cadddr GoogleDoc)
  )
;---------------------------------------------------
; FUNCIONES MODIFICADORES
;---------------------------------------------------
; Función que actualiza el stack.
; Dominio: user x stackUsers x docCreate x docAdd
; Recorrido: stack.
(define (actualGoogleDoc actualLogin users create addQ)
  (list actualLogin users create addQ)
  )
;---------------------------------------------------
; FUNCIONES MODIFICADORES
;---------------------------------------------------
; Función que comprueba si el usuario está activo.
; Dominio: doc
; Recorrido: boolean
(define (sesion? doc)
  (not (null? (GoogleDoc->loginActual doc)))
  )
