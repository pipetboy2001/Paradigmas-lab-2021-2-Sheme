#lang racket
(provide (all-defined-out))
(require "Registro.rkt")

; FUNCIONES PRINCIPALES

;Constructor paradigmadocs
;String X Date X EncryptFunction X DecryptFunction
;(paradigmadocs name date encryptFunction decryptFunction)
;Integer X Integer X Integer
;(date dd mm yyyy)

;Añadir pronto xd

;register
;Función que permite registrar a un nuevo usuario en la plataforma de documentos. Para esto se solicita la plataforma de documentos, nombre del usuario (identificador único, se debe verificar que no exista para su correcto registro) y contraseña. El retorno de la función es una versión actualizada de paradigmadocs con el nuevo usuario registrado.
;recursion :Emplear recursión natur
;dom paradigmadocs X date X string X string
;paradigmadocs

(define (register doc Date username password )
  (if (Paradoc? doc)
      (if (sesion? doc)
       doc
       (if (and (string? username) (string? password)(date? Date))
          (if (not (userExist (Paradoc->users doc) username password))
           (actualParadoc (Paradoc->loginActual doc)
                 (userdoc (Paradoc->users doc) (newUser username password 0))
                 )
            doc
           )
          doc
       )
      )
      doc
      )
  )

