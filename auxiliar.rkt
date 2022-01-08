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

;Modificador
;restorar 
(define (restore number doc)
  (lambda (currentDoc) ;creacion currentDoc nuevo
              (actualGoogleDoc(endSesion)
                 (GoogleDoc->users doc) ;Cadr del doc
                 (createAct(GoogleDoc->Create doc) (create->id currentDoc) (create->author currentDoc) ;creacion de nueva lista 
                                   (- 1 (create->numAdd currentDoc)) ;bajar a 1
                                   (create->content currentDoc));llevar al contenido
                 (endSesion);cerramos sesion
                 )))

;---------------------------------------------------
; FUNCIONES SELECTORAS
;---------------------------------------------------
; Función que retorna el nombre del usuario.
; Dominio: stack.
; Recorrido: string.
(define (GoogleDoc->userLoginActual GoogleDoc)
  (car (car GoogleDoc))
  )
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
; Función que finaliza la sesion del usuario.
; Dominio: 
; Recorrido: list
(define (endSesion)
  '()
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
; Función que permite añadir preguntas al stack.
; Dominio: doc x docCreate x number x list x list x string
; Recorrido: docCreate
(define (docNewCreate doc docCreate id permisos createDate question)
  (if (null? docCreate)
      (newDoc id (GoogleDoc->userLoginActual doc) (votes 0 0) 0 permisos createDate createDate (isOpen) 0 0 question)
      (if (= 1 (length docCreate))
          (append (GoogleDoc->Create doc)
                  (newDoc (+ id 1) (GoogleDoc->userLoginActual doc) (votes 0 0) 0 permisos createDate createDate (isOpen) 0 0 question))
          (docNewCreate  doc (cdr docCreate) (+ id 1) permisos createDate question)
       )
      )
  )
; Función que cierra un stack.
; Dominio: list(endSesion) x list(doc)
; Recorrido: doc
(define (endGoogleDoc end-SesionError otherDoc)
  (list end-SesionError otherDoc)
  )

; Función que permite responder una pregunta.
; Dominio: doc x docCreate x number x list x list x string
; Recorrido: docAdd
(define (AddAddQ docAdd idAdd userActual permisos date addContent idQuestion)
    (if (= idQuestion 1)
      (if (equal? '(()) (car docAdd))
          (cons (newAddDoc (newAdds idAdd userActual permisos date #f 0 (votes 0 0) addContent)) (cdr docAdd))
          (cons (append (car docAdd) (newAddDoc (newAdds idAdd userActual permisos date #f 0 (votes 0 0) addContent))) (cdr docAdd))
       )
      (cons (car docAdd) (AddAddQ (cdr docAdd) idAdd userActual permisos date addContent (- idQuestion 1)))
      )
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

  ;;string

  ; Función que convierte en string la sesion del usuario.
; Dominio: doc
; Recorrido: string
(define (loginActual->string user)
  (if (null? user)
        "No found users actives in GoogleDoc.\n"
        (string-append "Sesion activa\n" "Username: " (user->username user) "\n"
                                             "Permiso: " (number->string (user->reputation user)) "\n"
                                             "\n"
        )
  )
 )

 ; Función que pasa un doc de usuarios a un string.
; Dominio: stackUsers
; Recorrido: string
(define (users->string users)
 (string-append  "Users register in GoogleDoc \n"
     (Doc->String user->string users)
  )
 )

 ; Función que pasa los elementos de un doc en string.
; Dominio: función - doc
; Recorrido: string
(define (Doc->String dataType->string doc)
  (if (null? doc)
      "No found any Doc register. \n"
      (string-append (dataType->string (car doc)) "\n" (Doc->String dataType->string (cdr doc)))
  )
 )
 (define (user->string doc)
  (string-append "User: " (user->username doc) "\n"
                 "Permiso: " (number->string (user->reputation doc)) "\n"
   )
 )

; Función que concatena un doc de respuestas a su respectiva pregunta.
; Dominio: stackQuestion x docAdd
; Recorrido: string
(define (create-add->string createDoc addDoc)
  (if (null? createDoc)
      ""
      (string-append
           (creates->string (car createDoc))
           (añadidos->string (car addDoc))
            "\n"
            (create-add->string (cdr createDoc) (cdr addDoc))
       )
  )
 )

; Función que pasa la informacion de una respuesta a un string.
; Dominio: answer(list)
; Recorrido: string
(define (añadir->string añadido)
  (if (null? añadido)
         "Añadiendo a los doc. \n"
         (string-append "que doc? id: " (number->string (add->id añadido)) " Author: " (add->author añadido) "\n"
                 "  '' " (add->content añadido) " ''\n"
                 "\t Enviada el: " (date->string (add->date añadido))
                 "\t Permisos " ((lambda (permisoss) (if (equal? '("") permisoss) "No found tags. \n" (permisos->string permisoss)))(add->permisos añadido))
                
                 "\t" ((lambda (state) (if state "create accepted." "create no accepted."))(add->state añadido)) "\n"
                 )
   )
 )

; Función que pasa la informacion de una fecha a string.
; Dominio: list
; Recorrido: string
(define (date->string date)
  (reduce string-append (map (lambda (a) (if (> a 31) (string-append (number->string a)) (string-append (number->string a) "-"))) date) "\n")
 )

; Función que pasa la informacion de las etiquetas a string.
; Dominio: list
; Recorrido: string
(define (permisos->string permisos)
  (reduce string-append (map (lambda (a) (string-append a " ")) permisos) "\n")
 )

; Función que pasa un doc de respuestas a String.
; Dominio: docAdd
; Recorrido: string
(define (añadidos->string answers)
  (if (null? answers)
         "No found añadidos. \n"
         (string-append "Añadir: \n"
            (Doc->String añadir->string answers)
          )
  )
 )

; Función que pasa la información de una pregunta a string.
; Dominio: question(list)
; Recorrido: string
(define (creates->string question)
  (string-append "Doc ID: " (number->string (create->id question)) "\n"
                 "Author: " (create->author question) "Views: " (number->string (create->views question)) "\n"
                 "Permisos: " ((lambda (tagg) (if (equal? '("") tagg) "Question no tags. \n" (permisos->string tagg)))(create->permisos question))
                 "Date: " (date->string (date->dateDoc (create->Date question)))
                 " '' " (create->content question) " ''\n"
                 "Last date: " (date->string (date->dateLast (create->Date question)))
                 ((lambda (state) (if (equal? "OPEN" state) "Question open." "Question closed."))(create->state question))
                 "Añadidos: " (number->string (create->numAdd question)) "\n"
                 )
 )

; Funcion que reduce a una unica salida los elementos de una lista.
; Dominio: function x list x id
; Recorrido: element
(define (reduce function list id)
 (if (null? list)
      id
      (function (car list) (reduce function (cdr list) id))
  )
)
