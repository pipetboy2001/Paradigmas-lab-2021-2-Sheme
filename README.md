Ejemplos de implementación de funciones pedidas por enunciado:
*******************************************************************
REGISTER
*******************************************************************
Para otros ejemplos se debe tener en cuenta que el nombre de usuario y contraseña
deben ser strings o provocará error, lo mismo al intentar registrar un usuario
ya registrado.

(define user1 (register (GoogleDoc) "Naomi" "123asd"))
(define user2 (register user1 "Damian" "1a2b3e"))
(define user3 (register user2 "Cami" "ascv34"))
(define user4 (register user3 "Jake" "er3av"))
******************************************************************
LOGIN
*******************************************************************
;Simplemente para mostrar que funciona el Registro.
login (register '()  "ola" "pass") "ola" "pass" share
login (register '()  "ola" "pass") "ola" "pass" create
login (register '()  "ola" "pass") "ola" "pass" Add

*******************************************************************
CREATE
*******************************************************************
Para los ejemplos se utilizan los usuarios registrados anteriormente, se utiliza la función login junto con la función create,
reutilizando los datos.

(define login1 (((login user4 "Naomi" "123asd" create)(date 10 12 2009))"este es mi primer doc creado"  "Juan"))
(define login2 (((login login1 "Jake" "er3av" create)(date 12 09 2020))"Como leer un archivo en C" "Juan" "Miguel"))
(define login3 (((login login2 "Damian" "1a2b3e" create)(date 13 12 2012))"Esta es mi tesis" "Naomi" "Jake" "Juan"))
(define login4 (((login login3 "Damian" "1a2b3e" create)(date 28 10 2016))"Laboratorio numero 5 de electro" ))

********************************************************************
String
********************************************************************
STRING
;Mostrar de manera visual el doc
(paradigmadocs->string (GoogleDoc))

********************************************************************
SHARE
******************************************************************
(define Share1 (((login user1 "Pipe" "123asd" share)(date 30 10 2020)) 2 ))
(define Share2 (((login user4 "Juan" "er3av" share)(date 23 10 2021)) 1 ))
(define Share3 (((login user2 "Damian" "1a2b3e" share)(date 23 03 2021)) 1 ))
(define Share4 (((login user3 "Cami" "ascv34" share)(date 25 11 2020)) 1 ))
**************************************************************************
ADD
**************************************************************************
;añade un nuevo texto al Doc creado
(define Añadir1 (((login user2 "Damian" "123asd" Add)(date 10 12 2009))"gracias por responder" ))
(define Añadir2 (((login user3 "Cami" "ascv34" Add)(date 10 12 2009))"aqui se pone mas" ))
(define Añadir3 (((login user4 "Juan" "er3av" Add)(date 10 12 2009))"muy buen trabajo! " ))
****************************************************************************
RESTORE
****************************************************************************
;Restorar a un punto anterior antes de ser editado
(define Restorar1 (((login user1 "Pipe" "123asd" restoreVersion)1)2 ))
(define Restorar2 (((login user2 "Damian" "123asd" restoreVersion)2)3 ))
(define Restorar3 (((login user3 "Cami" "ascv34" restoreVersion)3)4 ))
(define Restorar4 (((login user4 "Juan" "er3av" restoreVersion)1)4 ))

****************************************************************************
STRING
****************************************************************************
;Mostrar de manera visual el doc
(paradigmadocs->string (GoogleDoc))
(paradigmadocs->string user4)
