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
(define user1 (register '() (date 30 11 2009) "Pipe" "123asd" ))
(define user1 (register (Paradoc) (date 30 11 2009) "Pipe" "123asd" ))
(define user2 (register user1 (date 01 12 2018) "Damian" "1a2b3e" ))
(define user3 (register user2 (date 16 10 2003) "Cami" "ascv34" ))
(define user4 (register user3 (date 20 04 2029) "Juan" "er3av" ))
******************************************************************
Login

login (register '() (date 30 01 2001) "ola" "pass") "ola" "pass" share

*****************************************************************
CREAR
*
(define login1 (((login user1 "Pipe" "123asd" Crear)(date 10 12 2009))"nuevoDoc" "primera linea" ))
*
(define login1 (((login user1 "Pipe" "123asd" UsarDoc)(date 10 12 2009))"nuevoDoc" "primera linea" ))
*********************************************************************
SHARE

* (define Share1 (((login user1 "Pipe" "123asd" share)2 )(access "felipe" "r") ))

ADD
*(define comment1 (((login user4 "Juan" "er3av" Add)(date 10 12 2009))"gracias por responder" ))
-recordatorio para pipe en una parte haces el ID +1 AND y añades , para el de eleminar debes hacer el contrario 