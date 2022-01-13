Ejemplos de implementación de funciones pedidas por enunciado:
*******************************************************************
REGISTER
*******************************************************************
Para otros ejemplos se debe tener en cuenta que el nombre de usuario y contraseña
deben ser strings o provocará error, lo mismo al intentar registrar un usuario
ya registrado.

(define user0 (register '() (date 30 11 2009) "Pipeto" "123asd" ))
(define user1 (register (GoogleDoc) (date 30 11 2009) "Pipe" "123asd" ))
(define user2 (register user1 (date 01 12 2018) "Damian" "123asd" ))
(define user3 (register user2 (date 16 10 2003) "Cami" "ascv34" ))
(define user4 (register user3 (date 20 04 2029) "Juan" "er3av" ))
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

(define Create1 (((login user4 "Naomi" "123asd" create)(date 10 12 2009))"este es mi primer doc creado"  "Juan"))
(define Create2 (((login Create1 "Jake" "er3av" create)(date 12 09 2020))"Como leer un archivo en C" "Juan" "Miguel"))
(define Create3 (((login Create2 "Damian" "1a2b3e" create)(date 13 12 2012))"Esta es mi tesis" "Naomi" "Jake" "Juan"))
(define Create4 (((login Create3 "Damian" "1a2b3e" create)(date 28 10 2016))"Laboratorio numero 5 de electro" ))

********************************************************************
SHARE
******************************************************************
;compartir a otro usuario mediante R = Read , W=Write ,C=Comentar
(define Share1 (((login user2 "Damian" "123asd" share)2 )(access "Damian" "r") ))
(define Share2 (((login user3 "Cami" "ascv34" share)2 )(access "Cami" "w") ))
(define Share3 (((login user4 "Juan" "er3av"  share)2 )(access "Juan" "c") ))
(define Share4 (((login user1 "Pipe" "123asd" share)(date 30 10 2020)) 2 ))
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
SEARCH
****************************************************************************
(define busqueda1 (((login user1 "Pipe" "123asd" search)GoogleDoc ) "hola"))
(define busqueda2 (((login user2 "Damian" "123asd" search)GoogleDoc ) "hola2"))
(define busqueda3 (((login user4 "Juan" "er3av" search)GoogleDoc ) "hola3"))
****************************************************************************
STRING
****************************************************************************
;Mostrar de manera visual el doc
(paradigmadocs->string (GoogleDoc))
(paradigmadocs->string user4)
****************************************************************************
COMMENT
****************************************************************************
(define Comment1 (((login user4 "Naomi" "123asd" comment)(date 10 12 2009)) "Le comento al doc del usuario 4 ehe"))

