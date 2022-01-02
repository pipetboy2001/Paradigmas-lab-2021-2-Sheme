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