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

*******************************************************************
ASK
*******************************************************************
Para los ejemplos se utilizan los usuarios registrados anteriormente, se utiliza la función login junto con la función ask,
reutilizando los datos.

(define login1 (((login user4 "Naomi" "123asd" create)(date 10 12 2009))"Como crear un array en C" "C" "programacion" "arrays"))
(define login2 (((login login1 "Jake" "er3av" create)(date 12 09 2020))"Como leer un archivo en C" "C" "programacion" "archivo"))
(define login3 (((login login2 "Damian" "1a2b3e" create)(date 13 12 2012))"Como parar un bucle" "programacion" "Java" "bucles"))
(define login4 (((login login3 "Damian" "1a2b3e" create)(date 28 10 2016))"Como escribir hola mundo en Scheme" "programacion" "Scheme" "string"))

*******************************************************************
REWARD
*******************************************************************
Para el ejemplo el puntaje debe ser mayor a cero, con el fin de poder hacer una pregunta y ofrecer un puntaje, por ello se
crea un stack con puntuaciones ya agregadas.

(define loginRewardStack '(()
 (("Naomi" "123asd" 20) ("Damian" "1a2b3e" 9) ("Cami" "ascv34" 100) ("Jake" "er3av" 15))
 ((1 "Naomi" (0 0) 0 ("C" "programacion" "arrays") ((10 12 2009) (10 12 2009)) "OPEN" 0 0 "Como crear un array en C")
 (2 "Jake" (0 0) 0 ("C" "programacion" "archivo") ((12 09 2020) (12 09 2020)) "OPEN" 0 0 "Como leer un archivo en C"))
 ((()) (()) (()) (()))))

(define reward1 (((login loginRewardStack "Naomi" "123asd" reward) 2) 6))
(define reward2 (((login reward1 "Cami" "ascv34" reward) 3) 1))
(define reward3 (((login reward2 "Jake" "er3av" reward) 4) 2))

*******************************************************************
ANSWER
*******************************************************************
Para el ejemplo se utilizan etiquetas y tambien respuestas sin ellas, con el 
fin de comprobar el programa.

(define Add1 ((((login reward3 "Jake" "er3av" Add)(date 01 10 2020))4)"Con funciones especificas del lenguaje" "Scheme" "programacion"))
(define Add2 ((((login Add1 "Jake" "er3av" Add)(date 12 09 2020))1)"Puedes buscar ayuda en libros" "C" "programacion"))
(define Add3 ((((login Add2 "Naomi" "123asd" Add)(date 20 01 2010))1)"Requiere teoria" ""))
(define Add4 ((((login Add3 "Cami" "ascv34" Add)(date 10 12 2012))3)"Puedes apretar las teclas" "Java" "programacion"))
(define Add5 ((((login Add4 "Naomi" "123asd" Add)(date 20 09 2018))3)"Para parar un bucle se tiene que" "Java" "programacion"))
(define Add6 ((((login Add6 "Jake" "er3av" Add)(date 14 03 2020))3)"Has que el compilador lo detenga" "Java" "programacion"))

*******************************************************************
ACCEPT
*******************************************************************
Para el ejemplo se aceptan respuestas de distintos usuarios otorgando puntajes correspondientes,
a cada uno.

(define accept1 ((login Add6 "Damian" "1a2b3e" accept)3)1))
(define accept2 ((login accept1 "Naomi" "123asd" accept)1)1))

*******************************************************************
STACK STRING
*******************************************************************
Para el ejemplo si el usuario no está conectado, la función devolverá el stack
sin realizar ninguna operación. Se prueba la función usando constructor inicial,
cuando o hay preguntas o no hay respuestas.

(stack->string (GoogleDoc))
*(login accept3 "Naomi" "123asd" stack->string)
(stack->string user4)
(stack->string reward2)
(stack->string accept3)

*******************************************************************
VOTE
*******************************************************************
(define vote1 ((((login accept3 "Naomi" "123asd" vote) getQuestion) 3) true))
(define vote2 ((((login vote1 "Damian" "1a2b3e" vote) getQuestion) 9) true))
(define vote3 ((((login vote2 "Cami" "ascv34" vote) getAnswer 2)) 1) false))
(define vote4 ((((login vote3 "Jake" "er3av" vote) getAnswer 1)) 2) true))
********************************************************************
SHARE
******************************************************************
(define Share1 (((login user1 "Pipe" "123asd" share)(date 30 10 2020)) 2 ))
(define Share2 (((login user4 "Juan" "er3av" share)(date 23 10 2021)) 1 ))
(define Share3 (((login user2 "Damian" "1a2b3e" share)(date 23 03 2021)) 1 ))
(define Share4 (((login user3 "Cami" "ascv34" share)(date 25 11 2020)) 1 ))
