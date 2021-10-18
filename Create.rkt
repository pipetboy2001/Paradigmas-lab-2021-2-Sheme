#lang racket
(provide (all-defined-out))
(require "Registro.rkt")

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
                  ;en caso que no
                  doc)
           ;en caso que no
           doc)
       ;cerrar parentesis del comienzo
       )))



;obtener el elemento [_,_,_,_,x]
(define (user->caddr user)
  (caddr user)
  )
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
;terminar sesion
(define (endSesion)
  '()
  )
;nuevo doc crear
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
;basandose en el lab del semestre pasado xd
(define (newCreate id author likes userList postingDate lastActivity follow numComment content)
  (list (list id author likes userList(dateCreate postingDate lastActivity) follow numComment content))
 )
(define (dateCreate postingDate lastActivity)
  (list postingDate lastActivity)
)

(define (Paradoc->Create doc)
  (caddr doc)
  )

