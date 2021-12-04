#lang racket
(provide (all-defined-out))
(require "Registro.rkt")

; REPRESENTACION
; paradigmadocs X date X String (nombreDoc) X String(contenido)
; EJEMPLOS:
; '() stack de pregunta vacío
; '(1 "pipe" 0 ("juan" "nagito" "miku") (23 11 2009) "S I haha meme.")
; '((1 "nagito" 0 ("pipe" "sol" "ana")(04 05 2022) "mis ojos solo miran su bellaza"))
; '((2 "juan" 0 ("pipe" "nagito" "tomi") (23 01 2001) "Por que jujutsu lo mas grande?"))

;------------------------
;PERTENENCIA
;------------------------
;funcion que comprueba que los datos existen y al final aplica la funcion
(define (comprobarLogin doc username password action)(if (user? (userExist (Paradoc->users doc) username password)) ; si el usuario existe dentro del doc 
          (action (actualParadoc (newUser username password (user->caddr (userExist (Paradoc->users doc) username password))) ;aplicar accion en una nueva lista 
                           ;funciones...
                           (Paradoc->Create doc) )) ; crear doc     
          (action doc) ;aplicar funcion
  ))


;nombre valido?
(define (nameDoc? nameDoc)
  (if (null? nameDoc) 
      #t
      (if (string? (car nameDoc))
          (nameDoc? (cdr nameDoc))
          #f
       )
  )
 )

(define (compruebaCreate doc Create CrearDate nameDoc)
  (if (and (string? Create) (date? CrearDate) (nameDoc? nameDoc)) ;si create es un string , createdate una fecha y el nombre del doc un nombre 
                  (actualParadoc (endSesion) ;colocar a la session
                                 (Paradoc->users doc);obtener el usuario
                                 (docnewCrear doc (Paradoc->Create doc) 1 nameDoc CrearDate Create));craer nuevo doc ingresando los parametros que tiene un doc
                                                                                                    ;ese 1 sera el ID para el add
                  ;en caso que no
                  doc)
  )

;________________________
;CONSTRUCTOR
;________________________

;terminar sesion
(define (endSesion)
  '()
  )
;nuevo doc crear
(define (docnewCrear doc docCreate id userList CrearDate Create)
  (if (null? docCreate)
      (newCreate id (Paradoc->userLoginActual doc) 0 userList CrearDate CrearDate 0 Create) ;id - usaurios - crear fecha 
      (if (= 1 (length docCreate))
          (append (Paradoc->Create doc)
                  (newCreate (+ id 1) (Paradoc->userLoginActual doc) 0 userList CrearDate CrearDate 0 Create))
          (docnewCrear  doc (cdr docCreate) (+ id 1) userList CrearDate Create)
       )
      )
  )


;fecha de creacion
(define (dateCreate postingDate lastActivity)
  (list postingDate lastActivity)
)

;basandose en el lab del semestre pasado xd
;neueva creacion , id , autor , creacion fecha - ultima actividad ,contenido
(define (newCreate id author   createDate lastActivity  content)
  (list (list id author  (dateCreate createDate lastActivity)  content))
 )
;------------------------
;MODIFICADORES
;-----------------------
;CREAR OWO
;Función que permite a un usuario con sesión iniciada en la plataforma crear un nuevo documento. Cada documento registra el autor del mismo (obtenido desde la sesión activa), fecha de creación (tipo Date), el nombre del documento y el contenido del documento (solo debe ser un string). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el nuevo documento y se elimina la sesión activa del usuario en paradigmadocs.
;dominio paradigmadocs X date X String (nombreDoc) X String(contenido)
;recorrido paradigmadocs
;ejemplo (login Paradoc “user” “pass” create) (date 30 10 2020) “doc1” “este es mi primer documento”) 
(define (Crear doc) ;se le ingresa el parametro de doc
   (lambda (CrearDate) ;una fecha
     (lambda (Create . nameDoc);nombre del doc 
       (if (sesion? doc) ;esta activo?
           (compruebaCreate doc Create CrearDate nameDoc); revisar si funcionan los datos
           ;finalmente al comprobarlo lo crea
           ;en caso que no
           doc)
       ;cerrar parentesis del comienzo
       )))


;------------------------
;SELECTORES
;------------------------
;obtener el elemento [_,_,_,_,x]
(define (user->caddr user)
  (caddr user)
  )

;obtener el login actual
(define (Paradoc->userLoginActual Paradoc)
  (car (car Paradoc))
  )
;obtener el create
(define (Paradoc->Create doc)
  (caddr doc)
  )

