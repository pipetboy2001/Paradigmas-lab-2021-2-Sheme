# Laboratorio1
 lab 1 de paradigmas 
Felipe fernandez H.
2057568-1

Ejemplos de implementación de funciones pedidas por enunciado:

*******************************************************************
REGISTER
*******************************************************************
Para otros ejemplos se debe tener en cuenta que el nombre de usuario y contraseña
deben ser strings o provocará error, lo mismo al intentar registrar un usuario
ya registrado.
(define user0 (register '() (date 30 11 2009) "Pipeto" "123asd" ))
(define user1 (register (Paradoc) (date 30 11 2009) "Pipe" "123asd" ))
(define user2 (register user1 (date 01 12 2018) "Damian" "123asd" ))
(define user3 (register user2 (date 16 10 2003) "Cami" "ascv34" ))
(define user4 (register user3 (date 20 04 2029) "Juan" "er3av" ))

******************************************************************
LOGIN
*******************************************************************
;Simplemente para mostrar que funciona.
login (register '() (date 30 01 2001) "ola" "pass") "ola" "pass" share
login (register '() (date 30 01 2001) "ola" "pass") "ola" "pass" Crear
login (register '() (date 30 01 2001) "ola" "pass") "ola" "pass" Add

*****************************************************************
CREAR
*******************************************************************
(define create1 (((login user2 "Damian" "123asd" Crear)(date 10 12 2009))"nuevoDoc" "primera linea" ))
(define create2 (((login user3 "Cami" "ascv34" UsarDoc)(date 10 12 2009))"nuevoDoc" "otra primera linea" ))
(define create3 (((login user4 "Juan" "er3av" UsarDoc)(date 10 12 2009))"nuevoDoc" "otra linea mas" ))

*********************************************************************
SHARE
*******************************************************************
(define Share1 (((login user2 "Damian" "123asd" share)2 )(access "Damian" "r") ))
(define Share2 (((login user3 "Cami" "ascv34" share)2 )(access "Cami" "w") ))
(define Share3 (((login user4 "Juan" "er3av"  share)2 )(access "Juan" "c") ))

**************************************************************************
ADD
**************************************************************************
(define Añadir1 (((login user2 "Damian" "123asd" Add)(date 10 12 2009))"gracias por responder" ))
(define Añadir2 (((login user3 "Cami" "ascv34" Add)(date 10 12 2009))"aqui se pone mas" ))
(define Añadir3 (((login user4 "Juan" "er3av" Add)(date 10 12 2009))"muy buen trabajo! " ))

****************************************************************************
RESTORE
****************************************************************************
(define Restorar1 (((login user1 "Pipe" "123asd" restoreVersion)1)2 ))
(define Restorar2 (((login user2 "Damian" "123asd" restoreVersion)2)3 ))
(define Restorar3 (((login user3 "Cami" "ascv34" restoreVersion)3)4 ))
(define Restorar4 (((login user4 "Juan" "er3av" restoreVersion)1)4 ))

**************************************************************************
RevokeAllAccesses
*ADVERTENCIA NO FUNCIONA*
(RevokeAllAccesses Paradoc) ;funciona por que si xd pero no aplica a lo que necesito
(define Remover1 (((login user1 "Pipe" "123asd" RevokeAllAccesses)1)2 ))
(define remove2 (((login user2 "Damian" "123asd" RevokeAllAccesses)2 )(access "felipe" "r") ))
(define remove3 (login Paradoc "Pipe" "123asd" revokeAllAccesses))
*****************************************************************************
SEARCH
(define busqueda1 (((login user1 "Pipe" "123asd" search)Paradoc ) "hola"))
(define busqueda1 (((login user2 "Damian" "123asd" search)Paradoc ) "hola"))
(define busqueda1 (((login user4 "Juan" "er3av" search)Paradoc ) "hola"))
***********************
STRING
(paradigmadocs->string (Paradoc))