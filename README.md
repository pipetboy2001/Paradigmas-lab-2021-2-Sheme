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

(define user1 (register (Paradoc) (date 30 11 2009) "Pipe" "123asd" ))
(define user2 (register user1 (date 01 12 2018) "Damian" "1a2b3e" ))
(define user3 (register user2 (date 16 10 2003) "Cami" "ascv34" ))
(define user4 (register user3 (date 20 04 2029) "Juan" "er3av" ))