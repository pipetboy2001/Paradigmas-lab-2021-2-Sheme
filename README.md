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
(define user0 (register '() (date 30 11 2009) "Pipe" "123asd" ))
(define user1 (register (Paradoc) (date 30 11 2009) "Pipe" "123asd" ))
(define user2 (register user1 (date 01 12 2018) "Damian" "1a2b3e" ))
(define user3 (register user2 (date 16 10 2003) "Cami" "ascv34" ))
(define user4 (register user3 (date 20 04 2029) "Juan" "er3av" ))
******************************************************************
Login
;Simplemente para mostrar que funciona.
login (register '() (date 30 01 2001) "ola" "pass") "ola" "pass" share
*****************************************************************
CREAR
(define create1 (((login user1 "Pipe" "123asd" Crear)(date 10 12 2009))"nuevoDoc" "primera linea" ))
(define create3 (((login user1 "Pipe" "123asd" UsarDoc)(date 10 12 2009))"nuevoDoc" "primera linea" ))
*********************************************************************
SHARE
(define Share1 (((login user1 "Pipe" "123asd" share)2 )(access "felipe" "r") ))

**************************************************************************
ADD
(define Añadir1 (((login user4 "Juan" "er3av" Add)(date 10 12 2009))"gracias por responder" ))
****************************************************************************
RESTORE
(define Restorar1 (((login user1 "Pipe" "123asd" restoreVersion)1)2 ))
**************************************************************************
RevokeAllAccesses
*ADVERTENCIA NO FUNCIONA*
(RevokeAllAccesses Paradoc) ;funciona por que si xd pero no aplica a lo que necesito
(define Remover1 (((login user1 "Pipe" "123asd" RevokeAllAccesses)1)2 ))
(define remove2 (((login user1 "Pipe" "123asd" RevokeAllAccesses)2 )(access "felipe" "r") ))
(define remove3 (login Paradoc "Pipe" "123asd" revokeAllAccesses))
*****************************************************************************
SEARCH
(define busqueda1 (((login user1 "Pipe" "123asd" search)Paradoc ) "hola"))
***********************
STRING
(paradigmadocs->string (Paradoc))