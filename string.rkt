#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
(require "anadir.rkt")

;----------------
;CONSTRUCTOR
;----------------
;conver string el usuario
(define (loginActual->string user)
  (if (null? user)
        "No se encuentran usuarios activos en Paradoc..\n"
        (string-append "Sesion activa\n" "Username: " (user->username user) "\n"
                                             "\n"
        )
  )
 )

;pasar user a string
(define (users->string users)
 (string-append  "Usuarios registrados en Paradoc \n"
     (Paradigmadocs->string  user->string users)
  )
 )
;pasar paradigmadoc a string
(define (Paradigmadocs->string  dataType->string doc)
  (if (null? doc)
      "No se encontró ningún registro. \n"
      (string-append (dataType->string (car doc)) "\n" (Paradigmadocs->string  dataType->string (cdr doc)))
  )
 )
;pasar user a string por doc
(define (user->string doc)
  (string-append "Usuarios: " (user->username doc) "\n"
   )
 )
;pasar fecha a string
(define (date->string date)
  (reduce string-append (map (lambda (a) (if (> a 31)
                                             (string-append (number->string a)
                                                            )
                                             (string-append (number->string a) "-")
                                             )
                               )
                             date)
          "\n")
 )
;pasar usuario a string
(define (userList->string userList)
  (reduce string-append (map (lambda (a) (string-append a " ")) userList) "\n")
 )



;concadenar versiones
(define (Create-Version->string CreatesStack VersionStack)
  (if (null? CreatesStack)
      ""
      (string-append
           (Create->string (car CreatesStack))
           (Version->string (car VersionStack))
            "\n"
            (Create-Version->string (cdr CreatesStack) (cdr VersionStack))
       )
  )
 )

;string el create
(define (Create->string Create)
  (string-append "Create ID: " (number->string (Create->id Create)) "\n"
                 "Author: " (Create->author Create) "Views: " (number->string) "\n"
                 "userList: " ((lambda (tagg) (if (equal? '("") tagg) "Create no userList. \n" (userList->string tagg)))(Create->userList Create))
                 "Date: " (date->string (date->dateCreate (Create->dates Create)))
                 " '' " (Create->content Create) " ''\n"
                 "Last date: " (date->string (date->dateLast (Create->dates Create))))
                  "\n"
                 "Version: " (number->string (Create->numVersion Create)) "\n"
                 
                  "\n"
                 )



;versionamiento
(define (Paradoc->VersionQ Paradoc)
  (cadddr Paradoc)
  )
;string las versiones
(define (Version->string Version)
  (if (null? Version)
         "Create without Version. \n"
         (string-append "Version id: " (number->string (Version->id Version)) " Author: " (Version->author Version) "\n"
                 "  '' " (Version->content Version) " ''\n"
                 "\t Enviado el: " (date->string (Version->date Version))
                 "\t userList " ((lambda (tagg) (if (equal? '("") tagg) "No found userList. \n"
                                                    (userList->string tagg)))(Version->userList Version))
                
                 )
   )
 )

;----------------
;MODIFICADORES
;----------------
;reducir a una unica salida de elementos de una lista
(define (reduce function list id)
 (if (null? list)
      id
      (function (car list) (reduce function (cdr list) id))
  )
)

;----------------
;SELECTORES
;---------------
;version id
(define (Version->id Version)
  (car Version)
  )
;version autor
(define (Version->author Version)
  (cadr Version)
  )
;version contenido
(define (Version->content Version)
  (cadr (cdddr (cdddr Version)))
  )
;version fecha
(define (Version->date Version)
  (cadddr Version)
  )
;version userlist
(define (Version->userList Version)
  (caddr Version)
  )
;fecha creada
(define (date->dateCreate date)
  (car date)
  )
;fecha 
(define (Create->dates Create)
  (cadr (cdddr Create))
 )
;creacion contenido
(define (Create->content Create)
  (caddr (cdddr (cdddr Create))
  ))
;fecha final
(define (date->dateLast date)
  (cadr date)
  )
;numero de version
(define (Create->numVersion Create)
  (cadr (cdddr (cdddr Create)))
 )
;autor
(define (Create->author Create)
  (cadr Create)
 )
;lista de usuarios
(define (Create->userList Create)
  (cadddr  Create)
 )

