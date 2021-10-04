#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "fecha.rkt")

; FUNCIONES PRINCIPALES

;Constructor paradigmadocs
;String X Date X EncryptFunction X DecryptFunction
;(paradigmadocs name date encryptFunction decryptFunction)
;Integer X Integer X Integer
;(date dd mm yyyy)

;Añadir pronto xd
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;register
;Función que permite registrar a un nuevo usuario en la plataforma de documentos. Para esto se solicita la plataforma de documentos, nombre del usuario (identificador único, se debe verificar que no exista para su correcto registro) y contraseña. El retorno de la función es una versión actualizada de paradigmadocs con el nuevo usuario registrado.
;recursion :Emplear recursión natur
;dom paradigmadocs X date X string X string
;paradigmadocs

(define (register doc Date username password )
  (if (Paradoc? doc) ;si -paradoc exite?
      (if (sesion? doc) ;si secion activa?
          doc ;devolver doc
          (if (and (string? username) (string? password)(date? Date)) ;si el nombre , contraeña son string y la fecha una fehca
              (if (not (userExist (Paradoc->users doc) username password)) ;si el nombre no es igual a la contraseña
                  (actualParadoc (Paradoc->loginActual doc) ;en la lista de usuario
                                 (userdoc (Paradoc->users doc) (newUser username password 0))) ;añadir al usuario
                  ;los else aqui abajo xd
                  doc)
          ;en caso que no
          doc)
       ;en caso que no fuera
       )
      ;en caso que no doc
      doc)
  ;en caso que no te vas aqui
  )

;------------------------------------------------------------------------------------------------------------------------------------------------------------------