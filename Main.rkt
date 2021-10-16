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
;login
;Función que permite autenticar a un usuario registrado iniciar sesión y junto con ello permite la ejecución de comandos concretos dentro de la plataforma. Si la autenticación es válida (i.e., que el usuario existe). El retorno es una función correspondiente a la operación (operation) parcialmente evaluada con el parámetro paradigmadocs actualizado. La actualización de paradigmadocs incorpora al usuario autenticado en la sesión activa (fundamental para que el comando pueda funcionar). De lo contrario retorna solo operation
;dominio paradigmadocs X string X string X function
;recorrido paradigmadocs
;example (login paradigmadocs “user” “pass” create)
(define (login doc username password action) ;primero definimos tiene el doc , nombre , contraseña , y la accion a realizar 
  (if (sesion? doc) ;la sesion que ingresa es valida?
      (action doc) ; aplicamos la accion
      (if (user? (userExist (Paradoc->users doc) username password)) ; si el usuario existe dentro del doc 
          (action (actualParadoc (newUser username password (user->Follow (userExist (Paradoc->users doc) username password))) ;aplicar accion en una nueva lista 
                           (Paradoc->users doc) ;usuario
                           (Paradoc->Create doc) )) ; crear doc     
          (action doc)
       )
  )
 )

;definir cosas despues tirar a otro archivo
(define (user->Follow user)
  (caddr user)
  )

(define (nameDoc? nameDoc)
  (if (null? nameDoc) 
      #t
      (if (string? (car nameDoc))
          (nameDoc? (cdr nameDoc))
          #f
       )
  )
 )

(define (endSesion)
  '()
  )

(define (docnewCrear doc docCreate id userList CrearDate Create)
  (if (null? docCreate)
      (newCreate id (Paradoc->userLoginActual doc) 0 userList CrearDate CrearDate 0 Create)
      (if (= 1 (length docCreate))
          (append (Paradoc->Create doc)
                  (newCreate (+ id 1) (Paradoc->userLoginActual doc) 0 userList CrearDate CrearDate 0 Create))
          (docnewCrear  doc (cdr docCreate) (+ id 1) userList CrearDate Create)
       )
      )
  )

(define (Paradoc->userLoginActual Paradoc)
  (car (car Paradoc))
  )

(define (newCreate id author likes userList postingDate lastActivity follow numComment content)
  (list (list id author likes userList(dateCreate postingDate lastActivity) follow numComment content))
 )
(define (dateCreate postingDate lastActivity)
  (list postingDate lastActivity)
)

(define (Paradoc->Create doc)
  (caddr Paradoc)
  )
;--------------------------------------------------------------------------------------------------
;CREAR OWO
;Función que permite a un usuario con sesión iniciada en la plataforma crear un nuevo documento. Cada documento registra el autor del mismo (obtenido desde la sesión activa), fecha de creación (tipo Date), el nombre del documento y el contenido del documento (solo debe ser un string). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el nuevo documento y se elimina la sesión activa del usuario en paradigmadocs.
;dominio paradigmadocs X date X String (nombreDoc) X String(contenido)
;recorrido paradigmadocs
;ejemplo (login Paradoc “user” “pass” create) (date 30 10 2020) “doc1” “este es mi primer documento”) 
(define (Crear doc) ;se le ingresa el parametro de doc
   (lambda (CrearDate) ;una fecha
     (lambda (Create . nameDoc);nombre del doc 
       (if (sesion? doc) ;esta activo?
           (if (and (string? Create) (date? CrearDate) (nameDoc? nameDoc)) ;si create es un string , createdate una fecha y el nombre del doc un nombre 
                  (actualParadoc (endSesion) ;colocar a la session
                                 (Paradoc->users doc);obtener el usuario
                                 (docnewCrear doc (Paradoc->Create doc) 1 nameDoc CrearDate Create));craer nuevo doc ingresando los parametros que tiene un doc
                  doc)
           doc)
       )))


;funcion de prueba para hacer funcionar el login...
(define (doble x)
   (* x 2))