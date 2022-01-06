#lang racket
(require "auxiliar.rkt")
(require "TDA_usuario.rkt")
(require "TDA_create.rkt")
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
;-------------------------------------------
; TDA Login
;-------------------------------------------
; Función que permite autenticar a un usuario registrado iniciar 
; sesión y junto con ello permite la ejecución de comandos concretos 
; dentro del stack.
; Dominio: list(doc)xstring(username)xstring(password)xfuncion(action).
; Recorrido: doc actualizado con nuevo usuario o sin modificar.

(define (login doc username password action)
  (if (sesion? doc)
      (action doc)
      (if (user? (userExist (GoogleDoc->users doc) username password))
          (action (actualGoogleDoc (newUser username password (user->reputation (userExist (GoogleDoc->users doc) username password)))
                           (GoogleDoc->users doc)
                           (GoogleDoc->Create doc)
                           (GoogleDoc->Add doc)
                      ))
          (action doc)
       )
  )
 )
;-------------------------------------------
; TDA create
;-------------------------------------------
; Función currificada que permite a un usuario con sesión iniciada 
; en la plataforma realizar una nueva pregunta.
; Recursion: Natural
; Dominio: list(doc)xlist(date)xstring(docCreado)strings(permisos).
; Recorrido: doc actualizado con nueva pregunta o sin modificar.

(define (create doc)
   (lambda (createDate)
     (lambda (docCreado . permisos)
       (if (sesion? doc)
           (if (and (string? docCreado) (date? createDate) (permiso? permisos))
                  (actualGoogleDoc (endSesion)
                             (GoogleDoc->users doc)
                             (docNewCreate doc (GoogleDoc->Create doc) 1 permisos createDate docCreado)
                             (append (GoogleDoc->Add doc)  (list (newAddDoc '())))
                   )
                  doc
        )
        doc
        )
      )
    )
  )
 ;-------------------------------------------
; TDA share
;------------------------------------------

(define (share stack)
  (lambda (shareDate) 
    (lambda (idPost)
      (lambda (post . numPost)
        (if (sesion? stack)
          (if (and (date? shareDate) (and (number? idPost) (> idPost 0))
                   (string? post) (permiso? numPost) )
           ((lambda (currentPost) 
              (actualGoogleDoc
                 (endSesion)
                 (GoogleDoc->users stack)
               )
             ))
           (endGoogleDoc (endSesion) (cdr stack))
           )
           (endGoogleDoc (endSesion) (cdr stack))
        )
       )
     )
   )
)
;-------------------------------------------
; TDA Agregar
;-------------------------------------------
; Función currificada que permite a un usuario con sesión iniciada 
; en la plataforma responder una pregunta.
; Recursion: Natural
; Dominio: list(doc)xlist(date)xnumber(idQuestion)xstring(content Add)xstrings(permisos).
; Recorrido: doc actualizado. 

(define (Add doc)
  (lambda (addDate)
    (lambda (idCreate)
      (lambda (Add . permisos)
        (if (sesion? doc)
          (if (and (date? addDate) (and (number? idCreate) (> idCreate 0))
                   (string? Add) (permiso? permisos) (not (equal? false (createExist (GoogleDoc->Create doc) idCreate))))
           ((lambda (currentCreate) 
              (actualGoogleDoc
                 (endSesion)
                 (GoogleDoc->users doc)
                 (createAct   (GoogleDoc->Create doc) (create->id currentCreate) (create->author currentCreate)
                                   (votes->a (create->votes currentCreate)) (votes->f (create->votes currentCreate))
                                   (+ 1 (create->views currentCreate))
                                   (create->permisos currentCreate) (date->dateDoc (create->Date currentCreate))
                                   addDate
                                   (create->state currentCreate) (create->reward currentCreate)
                                   (+ 1 (create->numAdd currentCreate))
                                   (create->content currentCreate)
                                  )
                 (AddAddQ (GoogleDoc->Add doc)
                                      (+ 1 (create->numAdd currentCreate))
                                      (GoogleDoc->userLoginActual doc)
                                      permisos
                                      addDate
                                      Add
                                      (create->id currentCreate)
                                      )
               )
             )(createExist (GoogleDoc->Create doc) idCreate))
           (endGoogleDoc (endSesion) (cdr doc))
           )
           (endGoogleDoc (endSesion) (cdr doc))
        )
       )
     )
   )
)

;---RESTORE Version ---
;Función que permite restaurar una versión anterior de un documento.
;Como resultado de esta función, la versión activa pasa a ser una versiónv mas dentro del historial
;y la versión restaurada pasa a ser la versión activa del documento. Retorno final de la función es una versión actualizada de paradigmadocs
;donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM:paradigmadocs X int X int
;REC paradigmadocs

(define (restoreVersion doc)
  (lambda (idN)
    (lambda (idA)
    (if (and (number? idA) (number? idN) )
        (restore (- 1) idA) ;restorar al anterior , -1 es para volver 
        ;else
        doc
    ))))

;-------------------------------------------------------------------------------------------------------------------------------------------------- 
;RevokeAllAccesses


;--------------------------------------------------------------------------------------------------------------------------------------------------
;search

;-------------------------------------------
; TDA paradigmadocs->string
;-------------------------------------------
; Función que recibe un doc y entrega una representación del mismo 
; como un string posible de visualizar de forma comprensible al usuario.
; Recursion: Natural
; Dominio: list(doc).
; Recorrido: string.

(define (paradigmadocs->string doc)
  (string-append
   (loginActual->string (GoogleDoc->loginActual doc))
   (users->string (GoogleDoc->users doc))
   (create-add->string (GoogleDoc->Create doc) (GoogleDoc->Add doc))
  )
 )
