#lang racket
(provide (all-defined-out))
(require "fecha_20575068_FernandezHernandez.rkt")
;--------------------------
;REPRESENTACION
;-------------------------
; (user username password )
; '() stack vacio

; EJEMPLO TDA USUARIO:
; '("pipe" "asdf34" )
; '(("pipe" "asdf34 ) ("Juan" "qwert43" ))

;-------------------------
;PERTENENCIA
;------------------------
;primero una funcion para ver si cumple que es un doc de paradigmas xd
(define (Paradoc? Paradoc)
  (if (list? Paradoc)
      (if (= 4 (length Paradoc))
          (if (and (user? (Paradoc->loginActual Paradoc))
                   (validdoc user? (Paradoc->users Paradoc))
                   ;(validdoc question? (Paradoc->Post Paradoc))
                   ;(validdoc comment? (Paradoc->commentQ Paradoc))
               )
              #t
              #f
              )
          #f
       )
      #f
   )
  )

(define estaUser? (lambda (usuario registro)
                        (if(null? registro)
                           #f
                           (if(equal? usuario (getUser registro))
                              #t
                              (estaUser? usuario (getUserSiguiente registro))))))


;cumple que es un usuario?
(define (user? user)
  (if (null? user)
  #t
  (if (list? user)
      (if (= 3 (length user))
          (if (and (string? (car user)) ;primer elemento strig
                   (string? (car (cdr user)));segundo elemento string
                   (number? (car (cdr (cdr user))))) ;tercer elemento numero
              #t
              #f
           )
          #f
       )
      #f
    )
  )
  )

;validar el doc
(define (validdoc validation doc2)
  (if (null? doc2) ;si esta vacio nice
      #t
      (if (validation (car doc2)) ;re
          (validdoc validation (cdr doc2))
          #f
          )
      )
  )


;inica sesion?
(define (sesion? doc)
  (not (null? (Paradoc->loginActual doc)))
  )

;que su nombre y contraseña no sea la misma >:[
(define (userExist docUsers username password)
  (if (null? docUsers)
      #f
      (if (and (equal? username (user->username (car docUsers)))
               (equal? password (user->password (car docUsers))))
      (car docUsers)
      (userExist (cdr docUsers) username password)
       )
      )
  )

;como es un actual paradoc
(define (actualParadoc actualLogin users )
  (list actualLogin users )
  )

;-----------------------
;CONSTRUCTOR
;-----------------------

;definamos que es un paradoc
(define (Paradoc)
  '(() () () ())
  )

;comprueba el registre
(define (comprobarRegistre doc Date username password)
   (if (and (string? username) (string? password)(date? Date)) ;si el nombre , contraeña son string y la fecha una fehca
              (if (not (userExist (Paradoc->users doc) username password)) ;si el nombre no es igual a la contraseña
                  (registrando(doc username password)) ;aplicar la funcion de añadir 
                  ;los else aqui abajo xd
            doc)doc))
                          

;usuarios del doc
(define (userdoc usersdoc addUser)
  (if (user? addUser)
    (if (null? usersdoc)
      (list addUser)
      (if (= 1 (length usersdoc))
          (cons (car usersdoc) (list addUser))
          (cons (car usersdoc) (userdoc (cdr usersdoc) addUser))
       )
    )
  usersdoc
  )
 )

;finalizar y mandar a otro doc
;(char<? #\r)
;(char<? #\w)
;(char<? #\c)

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

(define (access.accesses)
  access)
;-----------------------
;MODIFICADORES
;-----------------------
;funcion que añade al doc un usuario
(define (registrando doc username password)
  ((actualParadoc (Paradoc->loginActual doc) ;en la lista de usuario
                  (userdoc (Paradoc->users doc) (newUser username password 0))))) ;añadir al usuario ;el 0 sera por el add osea su paramtro de editor o no
                                                

(define addInicio(lambda(objeto lista)
                   (if (null? lista)
                       (cons objeto lista)
                       (cons objeto lista))))

;definamos los nuevos usuarios 
(define (newUser user password )
  (list user password )
  )

;-------------------------------
;SELECTORES
;--------------------------------
(define getUser car)
(define getUserSiguiente cdr)

;obtener el usuario
(define (Paradoc->users Paradoc)
  (cadr Paradoc)
  )

;ver el login
(define (Paradoc->loginActual Paradoc)
  (car Paradoc)
  )

;obtener el nombre

(define (user->username user)
  (car user)
  )

;obtener la contraseña
(define (user->password user)
  (car (cdr user))
  )


