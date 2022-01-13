#lang racket
(require "auxiliar.rkt")
(require "TDA_usuario.rkt")
(require "TDA_create.rkt")
(require "TDA_anadir.rkt")
(require "TDA_buscar.rkt")
(require "TDA_comentario.rkt")

;LAB FELIPE 20575068-1

; FUNCIONES PRINCIPALES
;-------------------------------------------
; TDA Register
;-------------------------------------------
; Función que permite registrar a un nuevo usuario en el stack.
; Recursion: Natural
; Dominio: list(doc)xstring(username)xstring(password)
; Recorrido: doc actualizado con nuevo usuario o sin modificar.
;Example:
;(define user1 (register (GoogleDoc) (date 15 06 2001) "Naomi" "123asd"))
;(define user2 (register user1 (date 30 02 2001) "Damian" "1a2b3e"))
;(define user3 (register user2 (date 21 01 2022) "Cami" "ascv34"))
;(define user4 (register user3 (date 01 02 2003) "Jake" "er3av"))

                        

(define (register doc date username password)
  (if (GoogleDoc? doc)
      (if (sesion? doc)
          doc
          (if (and (string? username) (string? password) )
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
;Example
;login (register '()  "ola" "pass") "ola" "pass" share
;login (register '()  "ola" "pass") "ola" "pass" create
;login (register '()  "ola" "pass") "ola" "pass" Add


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
; en la plataforma crear un nuevo documento
; Recursion: Natural
; Dominio: paradigmadocs X date X String (nombreDoc) X String (contenido)
; Recorrido: doc actualizado con nuevo doc o sin modificar.
;example
;(define login1 (((login user4 "Naomi" "123asd" create)(date 10 12 2009))"este es mi primer doc creado"  "Juan"))
;(define login2 (((login login1 "Jake" "er3av" create)(date 12 09 2020))"Como leer un archivo en C" "Juan" "Miguel"))
;(define login3 (((login login2 "Damian" "1a2b3e" create)(date 13 12 2012))"Esta es mi tesis" "Naomi" "Jake" "Juan"))
;(define login4 (((login login3 "Damian" "1a2b3e" create)(date 28 10 2016))"Laboratorio numero 5 de electro" ))

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
; Función que permite compartir un documento con otros usuarios
;especificando el tipo de acceso a éste
;DOM  paradigmadocs X int X access List
;REC paradigmadocs
;compartir a otro usuario mediante R = Read , W=Write ,C=Comentar
;(define Share1 (((login user2 "Damian" "123asd" share)2 )(access "Damian" "r") ))
;(define Share2 (((login user3 "Cami" "ascv34" share)2 )(access "Cami" "w") ))
;(define Share3 (((login user4 "Juan" "er3av"  share)2 )(access "Juan" "c") ))

;finalizar y mandar a otro doc
;(char<? #\r)
;(char<? #\w)
;(char<? #\c)
(define (share Doc)
  (lambda (shareDate) 
    (lambda (idDoc)
      (lambda (doc . numDoc)
        (if (sesion? Doc)
            (if (and (date? shareDate) (and (number? idDoc) (> idDoc 0))
                     (string? doc) (permiso? numDoc) )
                ((lambda (currentDoc) 
                   (actualGoogleDoc
                    (endSesion)
                    (GoogleDoc->users Doc)
                    )
                   ))
                (endGoogleDoc (endSesion) (cdr Doc))
                )
            (endGoogleDoc (endSesion) (cdr Doc))
            )
        )
      )
    )
  )
;-------------------------------------------
; TDA Agregar
;-------------------------------------------
; Función currificada que permite a un usuario con sesión iniciada 
; en la plataforma añadir texto al final de la versión actual/activa del documento.
; Recursion: Natural
; Dominio: list(doc)xlist(date)xnumber(idQuestion)xstring(content Add)xstrings(permisos).
; Recorrido: doc actualizado.
;example
;(define Añadir1 (((login user2 "Damian" "123asd" Add)(date 10 12 2009))"gracias por responder" ))
;(define Añadir2 (((login user3 "Cami" "ascv34" Add)(date 10 12 2009))"aqui se pone mas" ))
;(define Añadir3 (((login user4 "Juan" "er3av" Add)(date 10 12 2009))"muy buen trabajo! " ))

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
;-------------------------------------------
;RESTORE Version
;-------------------------------------------
;Función que permite restaurar una versión anterior de un documento.
;Como resultado de esta función, la versión activa pasa a ser una versiónv mas dentro del historial
;y la versión restaurada pasa a ser la versión activa del documento. Retorno final de la función es una versión actualizada de paradigmadocs
;donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM:paradigmadocs X int X int
;REC paradigmadocs
;example
;(define Restorar1 (((login user1 "Pipe" "123asd" restoreVersion)1)2 ))
;(define Restorar2 (((login user2 "Damian" "123asd" restoreVersion)2)3 ))
;(define Restorar3 (((login user3 "Cami" "ascv34" restoreVersion)3)4 ))
;(define Restorar4 (((login user4 "Juan" "er3av" restoreVersion)1)4 ))

(define (restoreVersion doc)
  (lambda (idN)
    (lambda (idA)
      (if (and (number? idA) (number? idN) )
          (restore (- 1) idA) ;restorar al anterior , -1 es para volver 
          ;else
          doc
          ))))

;-------------------------------------------
;RevokeAllAccesses
;-------------------------------------------
;Función que permite al usuario revocar todos los
;accesos a sus documentos. Retorno final de la función es una versión actualizada de
;paradigmadocs donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM paradigmadocs
;REC paradigmadocs
;(RevokeAllAccesses GoogleDoc)
;EXAMOLE
;(RevokeAllAccesses GoogleDoc)

(define (RevokeAllAccesses doc)
  (lambda (defined-Revoke)
    (if (sesion? doc)
     (and (defined-Revoke doc)(elimina doc defined-Revoke) ;copia y borra xd
      )
     (endGoogleDoc (endSesion) (cdr doc))
    )
    )
 )

;/**************************/
(define (elimina x Lista1)
(if (null? Lista1)
'()
(if (eq? x (car Lista1))
(elimina x (cdr Lista1))
(cons (car Lista1) (elimina x (cdr Lista1))))))

;-------------------------------------------
;search
;-------------------------------------------
;Función que permite al usuario buscar documentos (propios o que
;le hayan sido compartidos) que contengan un texto específico. La búsqueda se hace
;tanto en las versiones activas del documento como en las antiguas. Retorno final de
;la función es una lista de todos los documentos que contengan el texto especificado.
;DOM paradigmadoc
;REC document list
;(filter (lambda (x) (= x 5)) '(3 9 5 8 2 4 7))
;(memq '"hola" '("hola"))
;example
;(define busqueda1 (((login user1 "Pipe" "123asd" search)GoogleDoc ) "hola"))
;(define busqueda2 (((login user2 "Damian" "123asd" search)GoogleDoc ) "hola2"))
;(define busqueda3 (((login user4 "Juan" "er3av" search)GoogleDoc ) "hola3"))

(define (search doc) ;buscando en el doc
  (lambda (buscando) ; lo que buscara
    (lambda (list) ;creacion a nueva lista
      (lambda (correcto?) ;labda numero creado
        (buscandolo  buscando doc) ;metodo de busqueda
        ;fin
        (endGoogleDoc (endSesion) (cdr doc));finalizar
        ))
    )
  )



;-------------------------------------------
; TDA paradigmadocs->string
;-------------------------------------------
;Función que recibe una plataforma del tipo
;paradigmadocs y entrega una representación del mismo como un string posible de
;visualizar de forma comprensible al usuario
; Recursion: Natural
; Dominio: list (paradigmadocs).
; Recorrido: string.
;example
;(paradigmadocs->string (GoogleDoc))
;(paradigmadocs->string user4)

(define (paradigmadocs->string doc)
  (string-append
   (loginActual->string (GoogleDoc->loginActual doc))
   (users->string (GoogleDoc->users doc))
   (create-add->string (GoogleDoc->Create doc) (GoogleDoc->Add doc))
   )
  )

;-------------------------------------------
; TDA  delete
;-------------------------------------------
; Función que permite eliminar los últimos N caracteres de la versión
;actual/activa del documento
;Dom paradigmadocs X int X date X in
;REC paradigmadocs

;-------------------------------------------
; TDA searchAndReplace
;-------------------------------------------
;Función que permite buscar un texto en la versión
;actual/activa del documento y reemplazarlo por otro. La última versión del
;documento producto de cualquier cambio a través de esta función pasa a ser
;la versión activa.
;DOM paradigmadocs X int X date X String X String
;rec paradigmadocs

;-------------------------------------------
; TDA comment
;-------------------------------------------
;Función que permite comentar un texto seleccionado dentro del
;documento. Los comentarios se hacen sobre la versión activa del documento, sin
;embargo los comentarios van ligados tanto al documento como a la versión
;específica de éste

;DOM paradigmadocs X int X date X String X String
;REC paradigmadocs

;ejemplo de uso
;(define login1 (((login user4 "Naomi" "123asd" comment)(date 10 12 2009)) "Le comento al doc del usuario 4 ehe"))

(define comment (lambda(doc)
                (lambda(idCreate)
                  (lambda(fecha)
                    (lambda(comentario)
                        (if(equal? 3 (length doc))
                           (list (list (list (getUsername (getUser (getRegistro3 doc)))comentario idCreate)) (getCreate3 doc) (getUsuarioActivo3 doc) (getRegistro3 doc))
                           (if(equal? 4 (length doc))
                              (list (append (list (getUsername (getRegistro doc)) comentario idCreate) getComentario) (getComentario doc) (getUsuarioActivo doc) (getRegistro doc))
                              (doc))))))))

