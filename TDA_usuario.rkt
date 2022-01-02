#lang racket
(provide (all-defined-out))

;TDA USUARIO

;---------------------------------------------------
; FUNCION REPRESENTACION
;---------------------------------------------------
; (user username password reputation)
; '() stack vacio
; EJEMPLO TDA USUARIO:
; '("nagito" "asdf34" 0)
; '(("nagito" "asdf34 0) ("naomi" "qwwer43" 0)9

;---------------------------------------------------
; FUNCION CONSTRUCTOR
;---------------------------------------------------
; Funcion que permite crear un usuario.
; Dominio: string x string x number
; Recorrido: user(list)
(define (newUser user password reputation)
  (list user password reputation)
  )
;---------------------------------------------------
; FUNCION PERTENENCIA
;---------------------------------------------------
; Función que determina si el usuario se puede considerar un
; usuario.
; Dominio: user(list)
; Recorrido: boolean
(define (user? user)
  (if (null? user)
  #t
  (if (list? user)
      (if (= 3 (length user))
          (if (and (string? (car user))
                   (string? (car (cdr user)))
                   (number? (car (cdr (cdr user)))))
              #t
              #f
           )
          #f
       )
      #f
    )
  )
  )
;---------------------------------------------------
; FUNCIONES SELECTORAS
;---------------------------------------------------
; Función que retorna el nombre del usuario.
; Dominio: user(list)
; Recorrido: string
(define (user->username user)
  (car user)
  )
(define getUser (lambda (user)
                  (car user)))

; Función que retorna la password del usuario.
; Dominio: user(list)
; Recorrido: string
(define (user->password user)
  (car (cdr user))
  )

(define getPassword (lambda (user)
                  (car(cdr user))))
;---------------------------------------------------
; FUNCIONES MODIFICADORES
;---------------------------------------------------
; Función que agrega un usuario al stack.
; Dominio: stackUsers(list) x addUser(list).
; Recorrido: stackAc(list).
(define (userDoc usersDoc addUser)
  (if (user? addUser)
    (if (null? usersDoc)
      (list addUser)
      (if (= 1 (length usersDoc))
          (cons (car usersDoc) (list addUser))
          (cons (car usersDoc) (userDoc (cdr usersDoc) addUser))
       )
    )
  usersDoc
  )
 )
;---------------------------------------------------
; OTRAS FUNCIONES
;---------------------------------------------------
; Función que comprueba si un usuario y contraseña son iguales.
; Dominio: stackUsers(list)xstringxstring
; Recorrido: user(list) 
(define (userExist StackUsers username password)
  (if (null? StackUsers)
      #f
      (if (and (equal? username (user->username (car StackUsers)))
               (equal? password (user->password (car StackUsers))))
      (car StackUsers)
      (userExist (cdr StackUsers) username password)
       )
      )
  )