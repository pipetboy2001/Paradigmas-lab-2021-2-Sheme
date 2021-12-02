#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
(require "anadir.rkt")

(define (loginActual->string user)
  (if (null? user)
        "No se encuentran usuarios activos en Paradoc..\n"
        (string-append "Sesion activa\n" "Username: " (user->username user) "\n"
                                             "\n"
        )
  )
 )



(define (users->string users)
 (string-append  "Usuarios registrados en Paradoc \n"
     (Paradigmadocs->string  user->string users)
  )
 )
(define (Paradigmadocs->string  dataType->string doc)
  (if (null? doc)
      "No se encontró ningún registro. \n"
      (string-append (dataType->string (car doc)) "\n" (Paradigmadocs->string  dataType->string (cdr doc)))
  )
 )
(define (user->string doc)
  (string-append "Usuarios: " (user->username doc) "\n"
   )
 )

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

(define (date->dateCreate date)
  (car date)
  )
(define (Create->dates Create)
  (cadr (cdddr Create))
 )

(define (Create->content Create)
  (caddr (cdddr (cdddr Create))
  ))

(define (date->dateLast date)
  (cadr date)
  )
(define (Create->numVersion Create)
  (cadr (cdddr (cdddr Create)))
 )


(define (Create->author Create)
  (cadr Create)
 )
(define (Create->userList Create)
  (cadddr  Create)
 )

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

(define (userList->string userList)
  (reduce string-append (map (lambda (a) (string-append a " ")) userList) "\n")
 )

(define (reduce function list id)
 (if (null? list)
      id
      (function (car list) (reduce function (cdr list) id))
  )
)


(define (Paradoc->VersionQ Paradoc)
  (cadddr Paradoc)
  )

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

(define (Version->id Version)
  (car Version)
  )
(define (Version->author Version)
  (cadr Version)
  )
(define (Version->content Version)
  (cadr (cdddr (cdddr Version)))
  )

(define (Version->date Version)
  (cadddr Version)
  )

(define (Version->userList Version)
  (caddr Version)
  )


