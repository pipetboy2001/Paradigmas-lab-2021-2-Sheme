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
  (lambda (answerDate)
    (lambda (idQuestion)
      (lambda (Add . permisos)
        (if (sesion? doc)
          (if (and (date? answerDate) (and (number? idQuestion) (> idQuestion 0))
                   (string? Add) (permiso? permisos) (not (equal? false (createExist (GoogleDoc->Create doc) idQuestion))))
           ((lambda (currentQuestion) 
              (actualGoogleDoc
                 (endSesion)
                 (GoogleDoc->users doc)
                 (createAct   (GoogleDoc->Create doc) (create->id currentQuestion) (create->author currentQuestion)
                                   (votes->a (create->votes currentQuestion)) (votes->f (create->votes currentQuestion))
                                   (+ 1 (create->views currentQuestion))
                                   (create->permisos currentQuestion) (date->dateDoc (create->Date currentQuestion))
                                   answerDate
                                   (create->state currentQuestion) (create->reward currentQuestion)
                                   (+ 1 (create->numAdd currentQuestion))
                                   (create->content currentQuestion)
                                  )
                 (AddAddQ (GoogleDoc->Add doc)
                                      (+ 1 (create->numAdd currentQuestion))
                                      (GoogleDoc->userLoginActual doc)
                                      permisos
                                      answerDate
                                      Add
                                      (create->id currentQuestion)
                                      )
               )
             )(createExist (GoogleDoc->Create doc) idQuestion))
           (endGoogleDoc (endSesion) (cdr doc))
           )
           (endGoogleDoc (endSesion) (cdr doc))
        )
       )
     )
   )
)
