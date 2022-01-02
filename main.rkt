#lang racket
(require "auxiliar.rkt")
(require "TDA_usuario.rkt")
(require "TDA_anadir.rkt")

;LAB FELIPE 20575068-1

; FUNCIONES PRINCIPALES
;-------------------------------------------
; TDA Register
;-------------------------------------------
; Función que permite registrar a un nuevo usuario en el stack.
; Recursion: Natural
; Dominio: list(doc)xstring(username)xstring(password)
; Recorrido: doc actualizado con nuevo usuario o sin modificar.

(define (register doc username password)
  (if (GoogleDoc? doc)
      (if (sesion? doc)
       doc
       (if (and (string? username) (string? password))
          (if (not (userExist (GoogleDoc->users doc) username password))
           (actualGoogleDoc (GoogleDoc->loginActual doc)
                 (userDoc (GoogleDoc->users doc) (newUser username password 0))
                 (GoogleDoc->Create doc)
                 (GoogleDoc->Add doc)
                 )
            doc
           )
          doc
       )
      )
      doc
      )
  )