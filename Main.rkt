#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
(require "fecha.rkt")

; FUNCIONES PRINCIPALES

;Constructor paradigmadocs
;String X Date X EncryptFunction X DecryptFunction
;(paradigmadocs name date encryptFunction decryptFunction)
;Integer X Integer X Integer
;(date dd mm yyyy)

;Añadir pronto xd
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;---register---
;Función que permite registrar a un nuevo usuario en la plataforma de documentos. Para esto se solicita la plataforma de documentos, nombre del usuario (identificador único, se debe verificar que no exista para su correcto registro) y contraseña. El retorno de la función es una versión actualizada de paradigmadocs con el nuevo usuario registrado.
;recursion :Emplear recursión natur
;dom paradigmadocs X date X string X string
;paradigmadocs
;idea del mauri
;(define register (lambda (registro usuario pass)
;                   (if (estaUser? usuario registro)
;                       (append null registro)
;                       (if(equal? 1 (length registro))
;                          (addInicio (list usuario pass 10) registro)
;                          (addInicio (list usuario pass 10) registro)))))

(define (register doc Date username password )
  
  (if (Paradoc? doc) ;si -paradoc exite?
          doc ;devolver doc 
          (if (and (string? username) (string? password)(date? Date)) ;si el nombre , contraeña son string y la fecha una fehca
              (if (not (userExist (Paradoc->users doc) username password)) ;si el nombre no es igual a la contraseña
                  (actualParadoc (Paradoc->loginActual doc) ;en la lista de usuario
                  (userdoc (Paradoc->users doc) (newUser username password 0))) ;añadir al usuario ;el 0 sera por el add
                  ;los else aqui abajo xd
                  doc)doc)))

;------------------------------------------------------------------------------------------------------------------------------------------------------------------
;---login---
;Función que permite autenticar a un usuario registrado iniciar sesión y junto con ello permite la ejecución de comandos concretos dentro de la plataforma. Si la autenticación es válida (i.e., que el usuario existe). El retorno es una función correspondiente a la operación (operation) parcialmente evaluada con el parámetro paradigmadocs actualizado. La actualización de paradigmadocs incorpora al usuario autenticado en la sesión activa (fundamental para que el comando pueda funcionar). De lo contrario retorna solo operation
;dominio paradigmadocs X string X string X function
;recorrido paradigmadocs
;example (login paradigmadocs “user” “pass” create)
(define (login doc username password action) ;primero definimos tiene el doc , nombre , contraseña , y la accion a realizar 
      (action doc) ; aplicamos la accion
      (if (user? (userExist (Paradoc->users doc) username password)) ; si el usuario existe dentro del doc 
          (action (actualParadoc (newUser username password (user->caddr (userExist (Paradoc->users doc) username password))) ;aplicar accion en una nueva lista 
                           ;funciones...
                           (Paradoc->Create doc) )) ; crear doc     
          (action doc) ;aplicar funcion
  )
 )

;--------------------------------------------------------------------------------------------------
;----UsarDoc---
;Función que permite a un usuario con sesión iniciada en la plataforma crear un nuevo documento. Cada documento registra el autor del mismo (obtenido desde la sesión activa), fecha de creación (tipo Date), el nombre del documento y el contenido del documento (solo debe ser un string). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el nuevo documento y se elimina la sesión activa del usuario en paradigmadocs.
;dominio paradigmadocs X date X String (nombreDoc) X String(contenido)
;recorrido paradigmadocs
;ejemplo (login Paradoc “user” “pass” create) (date 30 10 2020) “doc1” “este es mi primer documento”) 
(define(UsarDoc doc)(Crear doc))
;--------------------------------------------------------------------------------------------------
;---share---
;Función que permite compartir un documento con otros usuarios especificando el tipo de acceso a éste (lectura, escritura, comentarios). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el tipo de acceso otorgado a cada usuario y se elimina la sesión activa del usuario en paradigmadocs.
;dom paradigmadocs X int X access List)
;rec paradigmadocs
;(share paradigmadocs idDoc access . accesses)
(define (share doc) ;comprtir el doc
  (lambda (idCreate) ; lamda id
    (lambda (access)
      (lambda (create . numCreate) ;labda numero creado
           (if (and (number? idCreate)) ;id es un numero?
           ((lambda (currentCreate) 
              (actualParadoc(endSesion)(Paradoc->users doc))));compartido
           (endParadoc (endSesion) (cdr doc))) ;finalizar
        ;y si no
        (endParadoc (endSesion) (cdr doc));finalizar
       ))))
;finalizar y mandar a otro doc
(define (endParadoc end-SesionError otherDoc)
  (list end-SesionError otherDoc)
  )
;acceso
(define (access user accion)
  (if accion = "r") ; posible funcion string?
  Paradoc->users accion
  (if accion = "w")
  Paradoc->users accion ;podria meter funcion create?
  (if accion = "c")
  Paradoc->users user ;posible funcion comment a futuro?
  )
;(char<? #\r)
;(char<? #\w)
;(char<? #\c)

;ADD:Función que permite añadir texto al final de la versión actual/activa del documento. La última versión del documento producto de cualquier cambio a través de esta función pasa a ser la versión activa. 
;Dom:paradigmadocs X int X date X String
;REC:paradigmadocs
(define (Add doc) 
  (lambda (AddDate);fecha
    (lambda (idDoc) ;id
      (if (sesion? doc);inicia session?
          (if (and (date? AddDate) (and (number? idDoc) (> idDoc 0))(string? Add))
          ;si la fecha es fecha xd y el id es un numero , y que el doc mayor a 0,y es un string lo que agregamos
           ((lambda (currentDoc) ;creacion currentDoc
              (actualParadoc(endSesion)
                 (Paradoc->users doc) ;Cadr del paradoc
                 (DocAct(Paradoc->Doc doc) (Doc->id currentDoc) (Doc->author currentDoc)AddDate ;creacion de nueva lista 
                                   (+ 1 (Doc->numAdd currentDoc)) ;AÑADIR +1
                                   (Doc->content currentDoc)) ;llevar al contenido
                 (AddAddQ (Paradoc->AddQ doc)(+ 1 (Doc->numAdd currentDoc)) ;añadir doc nuevo 
                          (Paradoc->userLoginActual doc)
                          AddDate ;fecha
                          Add     ;añadir
                          (Doc->id currentDoc))))) ;ahora hay que finalizar 
           (endParadoc (endSesion) (cdr doc)))
           (endParadoc (endSesion) (cdr doc))))))


(define (Paradoc->AddQ SocialNetwork)
  (cadddr SocialNetwork)
  )

(define (Doc->numAdd Post)
  (cadr (cdddr (cdddr Post)))
 )
(define (Doc->content Post)
  (caddr (cdddr (cdddr Post))
  ))


(define (AddAddQ docdocGrande idNuevaLinea userActual userList date NuevaLineaContent idPost)
    (if (= idPost 1)
      (if (equal? '(()) (car docdocGrande))
          (cons (newNuevaLineatack (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent)) (cdr docdocGrande))
          (cons (append (car docdocGrande) (newNuevaLineadoc (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent))) (cdr docdocGrande))
       )
      (cons (car docdocGrande) (AddAddQ (cdr docdocGrande) idNuevaLinea userActual userList date NuevaLineaContent (- idPost 1)))
      )
 )

(define ( comment)
  (list comment)
  )
(define (newNuevaLineatack comment)
  (list comment)
  )

(define (newNuevaLinea idcomment authorUser userList datecomment commentContent)
  (list idcomment authorUser userList datecomment commentContent)
  )
(define (newNuevaLineadoc  comment)
  (list comment)
  )
(define (Paradoc->Doc SocialNetwork)
  (caddr SocialNetwork)
  )
(define (Doc->id Post)
  (car Post)
 )
(define (Doc->author Post)
  (cadr Post)
 )

(define (Create->id create)
  (car create)
  )

(define (CreateNew id author userList PostingDate lastActivity follow numcomment content)
  (list id author userList numcomment content)
 )

;(define (DocAct docCreate idCreate author newLikeA newLikeF userList CreateDate newDate newFollow newAdd newContent)
  (define (DocAct docCreate idCreate author CreateDate newDate newAdd newContent)
    (if (null? docCreate)
      '() ;null
      (if (equal? idCreate (Create->id (car docCreate)))
          (cons (CreateNew idCreate
                              author
                              ;newLikeA
                              ;newLikeF
                              ;userList
                              CreateDate
                              newDate
                              ;newFollow
                              newAdd
                              newContent
                             )(cdr docCreate))
          (cons (car docCreate) (DocAct (cdr docCreate) idCreate author CreateDate newDate newAdd newContent))
          ;(cons (car docCreate) (DocAct (cdr docCreate) idCreate author newLikeA newLikeF userList CreateDate newDate newFollow newAdd newContent))
      )
    )
 )


; restoreVersion: Función que permite restaurar una versión anterior de un documento.
;Como resultado de esta función, la versión activa pasa a ser una versiónv mas dentro del historial
;y la versión restaurada pasa a ser la versión activa del documento. Retorno final de la función es una versión actualizada de paradigmadocs
;donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM:paradigmadocs X int X int
;REC paradigmadocs



;funcion de prueba para hacer pruebas del funcionamiento...
(define (doble x)
   (* x 2))

(define (UsarDoble x)(doble x))

