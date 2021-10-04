#lang racket
(provide (all-defined-out))
(require "fecha.rkt")

;okay empezemos...
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



;definamos que es un paradoc
(define (Paradoc)
  '(() () () ())
  )

;como es un actual paradoc
(define (actualParadoc actualLogin users )
  (list actualLogin users )
  )

;definamos los nuevos usuarios 
(define (newUser user password )
  (list user password )
  )


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


