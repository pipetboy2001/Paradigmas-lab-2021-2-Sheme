#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")
(require "fecha.rkt")
(require "compartir.rkt")
(require "anadir.rkt")
(require "restorarVersion.rkt")

; FUNCIONES PRINCIPALES

;Constructor paradigmadocs
;String X Date X EncryptFunction X DecryptFunction
;(paradigmadocs name date encryptFunction decryptFunction)
;Integer X Integer X Integer
;(date dd mm yyyy)
(define (paradigmadocs name date encryptFunction decryptFuction)
  (if (and (string? name)(date? date)) ;si el nombre es un string y la fecha igual 
      "Sesion activa\n" ;imprimir que la sesion esta activa c:
      ;no se si aqui es como el login que debe hacer funcionar una funcion
        (string-append "No se encuentran usuarios activos en Paradigmadocs..\n" ;si no que no se encuentra
        )
  )
 )

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;---register---
;Función que permite registrar a un nuevo usuario en la plataforma de documentos. Para esto se solicita la plataforma de documentos, nombre del usuario (identificador único, se debe verificar que no exista para su correcto registro) y contraseña. El retorno de la función es una versión actualizada de paradigmadocs con el nuevo usuario registrado.
;recursion :Emplear recursión natur
;dom paradigmadocs X date X string X string
;paradigmadocs

;funcion que comprueba si se añade usando registre
(define (comprobarRegistre doc Date username password)
   (if (and (string? username) (string? password)(date? Date)) ;si el nombre , contraeña son string y la fecha una fehca
              (if (not (userExist (Paradoc->users doc) username password)) ;si el nombre no es igual a la contraseña
                  (registrando(doc username password)) ;aplicar la funcion de añadir 
                  ;los else aqui abajo xd
            doc)doc))
                          
(define (register doc Date username password )
  (if (Paradoc? doc) ;si -paradoc exite?
          doc ;devolver doc
          (comprobarRegistre doc Date username password);comprobar que los datos esten bien y de paso añadirlo
          ))

;funcion que añade al doc un usuario
(define (registrando doc username password)
  ((actualParadoc (Paradoc->loginActual doc) ;en la lista de usuario
                  (userdoc (Paradoc->users doc) (newUser username password 0))))) ;añadir al usuario ;el 0 sera por el add osea su paramtro de editor o no
                                                
                   

;------------------------------------------------------------------------------------------------------------------------------------------------------------------



;---login---
;Función que permite autenticar a un usuario registrado iniciar sesión y junto con ello permite la ejecución de comandos concretos dentro de la plataforma. Si la autenticación es válida (i.e., que el usuario existe). El retorno es una función correspondiente a la operación (operation) parcialmente evaluada con el parámetro paradigmadocs actualizado. La actualización de paradigmadocs incorpora al usuario autenticado en la sesión activa (fundamental para que el comando pueda funcionar). De lo contrario retorna solo operation
;dominio paradigmadocs X string X string X function
;recorrido paradigmadocs
;example (login paradigmadocs “user” “pass” create)

;funcion que comprueba que los datos existen y al final aplica la funcion
(define (comprobarLogin doc username password action)(if (user? (userExist (Paradoc->users doc) username password)) ; si el usuario existe dentro del doc 
          (action (actualParadoc (newUser username password (user->caddr (userExist (Paradoc->users doc) username password))) ;aplicar accion en una nueva lista 
                           ;funciones...
                           (Paradoc->Create doc) )) ; crear doc     
          (action doc) ;aplicar funcion
  ))
                                                    

(define (login doc username password action) ;primero definimos tiene el doc , nombre , contraseña , y la accion a realizar 
      (action doc) ; aplicamos la accion
  (comprobarLogin doc username password action) ;funcion que comprueba que esta
  ;de paso esta aplica el action doc 
 )

;--------------------------------------------------------------------------------------------------
;----UsarDoc---
;Función que permite a un usuario con sesión iniciada en la plataforma crear un nuevo documento. Cada documento registra el autor del mismo (obtenido desde la sesión activa), fecha de creación (tipo Date), el nombre del documento y el contenido del documento (solo debe ser un string). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el nuevo documento y se elimina la sesión activa del usuario en paradigmadocs.
;dominio paradigmadocs X date X String (nombreDoc) X String(contenido)
;recorrido paradigmadocs
;ejemplo (login Paradoc “user” “pass” create) (date 30 10 2020) “doc1” “este es mi primer documento”) 
(define(UsarDoc doc)(Crear doc))
;--------------------------------------------------------------------------------------------------
;---share---
;Función que permite compartir un documento con otros usuarios especificando el tipo de acceso a éste (lectura, escritura, comentarios). El retorno final de la función es una versión actualizada de paradigmadocs donde se registra el tipo de acceso otorgado a cada usuario y se elimina la sesión activa del usuario en paradigmadocs.
;dom paradigmadocs X int X access List)
;rec paradigmadocs
;(share paradigmadocs idDoc access . accesses)
(define (share doc) ;comprtir el doc
  (lambda (idCreate) ; lamda id
    (lambda (access)
      (lambda (create . numCreate) ;labda numero creado
           (if (and (number? idCreate)) ;id es un numero?
           ((lambda (currentCreate) 
              (actualParadoc(endSesion)(Paradoc->users doc))));compartido
           (endParadoc (endSesion) (cdr doc))) ;finalizar
        ;y si no
        (endParadoc (endSesion) (cdr doc));finalizar
       ))))
;finalizar y mandar a otro doc
;(char<? #\r)
;(char<? #\w)
;(char<? #\c)
;---------------------------------------------------------------------------------------------------------------------------------------------------
;ADD:Función que permite añadir texto al final de la versión actual/activa del documento. La última versión del documento producto de cualquier cambio a través de esta función pasa a ser la versión activa. 
;Dom:paradigmadocs X int X date X String
;REC:paradigmadocs
(define (Add doc) 
  (lambda (AddDate);fecha
    (lambda (idDoc) ;id
      (if (sesion? doc);inicia session?
          (if (and (date? AddDate) (and (number? idDoc) (> idDoc 0))(string? Add))
          ;si la fecha es fecha xd y el id es un numero , y que el doc mayor a 0,y es un string lo que agregamos
           ((lambda (currentDoc) ;creacion currentDoc
              (actualParadoc(endSesion)
                 (Paradoc->users doc) ;Cadr del paradoc
                 (DocAct(Paradoc->Doc doc) (Doc->id currentDoc) (Doc->author currentDoc)AddDate ;creacion de nueva lista 
                                   (+ 1 (Doc->numAdd currentDoc)) ;AÑADIR +1
                                   (Doc->content currentDoc)) ;llevar al contenido
                 (AddAddQ (Paradoc->AddQ doc)(+ 1 (Doc->numAdd currentDoc)) ;añadir doc nuevo 
                          (Paradoc->userLoginActual doc)
                          AddDate ;fecha
                          Add     ;añadir
                          (Doc->id currentDoc))))) ;ahora hay que finalizar
           ;terminar al ser un if no cumplido.
           (endParadoc (endSesion) (cdr doc)))
           (endParadoc (endSesion) (cdr doc))))))

;--------------------------------------------------------------------------------------------------------------------------------------------------
; restoreVersion: Función que permite restaurar una versión anterior de un documento.
;Como resultado de esta función, la versión activa pasa a ser una versiónv mas dentro del historial
;y la versión restaurada pasa a ser la versión activa del documento. Retorno final de la función es una versión actualizada de paradigmadocs
;donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM:paradigmadocs X int X int
;REC paradigmadocs

(define (restoreVersion doc)
  (lambda (idN)
    (lambda (idA)
    (if (and (number? idA) (number? idN) )
        (restore (- 1) idA) ;restorar al anterior , -1 es para volver 
        ;else
        doc
    ))))


 
;RevokeAllAccesses
;Función que permite al usuario revocar todos los
;accesos a sus documentos. Retorno final de la función es una versión actualizada de
;paradigmadocs donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM paradigmadocs
;REC paradigmadocs

;FUNCION NO FUNCIONAL OMITIMOS

(define (RevokeAllAccesses doc)
  (lambda (defined-Revoke)
    (if (sesion? doc)
     (defined-Revoke doc)
     (endParadoc (endSesion) (cdr doc))
    )
    )
 )


;search
;Función que permite al usuario buscar documentos (propios o que
;le hayan sido compartidos) que contengan un texto específico. La búsqueda se hace
;tanto en las versiones activas del documento como en las antiguas. Retorno final de
;la función es una lista de todos los documentos que contengan el texto especificado.
;DOM paradigmadoc
;REC document list
;(filter (lambda (x) (= x 5)) '(3 9 5 8 2 4 7))
;(memq '"hola" '("hola"))

(define (search doc) ;buscando en el doc
  (lambda (buscando) ; lo que buscara
    (lambda (list) ;creacion a nueva lista
      (lambda (correcto?) ;labda numero creado
        (buscandolo  buscando doc) ;metodo de busqueda
        ;fin
        (endParadoc (endSesion) (cdr doc));finalizar
       ))
)
)


;empezar una busqueda
(define (buscandolo  buscando doc)
  (occurrence buscando doc);si se repire 
        (member buscando doc) ;ve el elemento 
           (endParadoc (endSesion) (cdr doc)));finalizar)

;cuantas veces ocurre
(define (occurrence x lst)
       (if (null? lst) 0    
            (if (equal? x (car lst))  (+ 1 (occurrence x (cdr lst)))
                                     (occurrence x (cdr lst)) 
            ))) 


;true or false si se encuentra 
(define (member? x list)
     (cond ((null? list) #f)
           ((equal? x (car list)) #t)
           (else   (member? x (cdr list)))))


;paradigmadocs->string:
;Función que recibe una plataforma del tipo
;paradigmadocs y entrega una representación del mismo como un string posible de
;visualizar de forma comprensible al usuario


(define (paradigmadocs->string  doc)
  (string-append
   (loginActual->string (Paradoc->loginActual doc))
   (users->string (Paradoc->users doc))
   (Create-Version->string (Paradoc->Create doc) (Paradoc->VersionQ doc))
  )
 )

(define (loginActual->string user)
  (if (null? user)
        "No se encuentran usuarios activos en Paradoc..\n"
        (string-append "Sesion activa\n" "Username: " (user->username user) "\n"
                                             "\n"
        )
  )
 )

(define (Paradoc->loginActual Paradoc)
  (car Paradoc)
  )
(define (user->username user)
  (car user)
  )
(define (users->string users)
 (string-append  "Usuarios registrados en Paradoc \n"
     (Paradigmadocs->string  user->string users)
  )
 )
(define (Paradigmadocs->string  dataType->string doc)
  (if (null? doc)
      "No se encontró ningún registro. \n"
      (string-append (dataType->string (car doc)) "\n" (Paradigmadocs->string  dataType->string (cdr doc)))
  )
 )
(define (user->string doc)
  (string-append "Usuarios: " (user->username doc) "\n"
   )
 )
(define (Paradoc->users Paradoc)
  (cadr Paradoc)
  )
(define (Create-Version->string CreatesStack VersionStack)
  (if (null? CreatesStack)
      ""
      (string-append
           (Create->string (car CreatesStack))
           (Version->string (car VersionStack))
            "\n"
            (Create-Version->string (cdr CreatesStack) (cdr VersionStack))
       )
  )
 )

(define (Create->string Create)
  (string-append "Create ID: " (number->string (Create->id Create)) "\n"
                 "Author: " (Create->author Create) "Views: " (number->string) "\n"
                 "userList: " ((lambda (tagg) (if (equal? '("") tagg) "Create no userList. \n" (userList->string tagg)))(Create->userList Create))
                 "Date: " (date->string (date->dateCreate (Create->dates Create)))
                 " '' " (Create->content Create) " ''\n"
                 "Last date: " (date->string (date->dateLast (Create->dates Create))))
                  "\n"
                 "Version: " (number->string (Create->numVersion Create)) "\n"
                 
                  "\n"
                 )

(define (date->dateCreate date)
  (car date)
  )
(define (Create->dates Create)
  (cadr (cdddr Create))
 )

(define (Create->content Create)
  (caddr (cdddr (cdddr Create))
  ))

(define (date->dateLast date)
  (cadr date)
  )
(define (Create->numVersion Create)
  (cadr (cdddr (cdddr Create)))
 )

(define (Create->id Create)
  (car Create)
 )
(define (Create->author Create)
  (cadr Create)
 )
(define (Create->userList Create)
  (cadddr  Create)
 )

(define (date->string date)
  (reduce string-append (map (lambda (a) (if (> a 31)
                                             (string-append (number->string a)
                                                            )
                                             (string-append (number->string a) "-")
                                             )
                               )
                             date)
          "\n")
 )

(define (userList->string userList)
  (reduce string-append (map (lambda (a) (string-append a " ")) userList) "\n")
 )

(define (reduce function list id)
 (if (null? list)
      id
      (function (car list) (reduce function (cdr list) id))
  )
)
(define (Paradoc->Create Paradoc)
  (caddr Paradoc)
  )

(define (Paradoc->VersionQ Paradoc)
  (cadddr Paradoc)
  )

(define (Version->string Version)
  (if (null? Version)
         "Create without Version. \n"
         (string-append "Version id: " (number->string (Version->id Version)) " Author: " (Version->author Version) "\n"
                 "  '' " (Version->content Version) " ''\n"
                 "\t Enviado el: " (date->string (Version->date Version))
                 "\t userList " ((lambda (tagg) (if (equal? '("") tagg) "No found userList. \n"
                                                    (userList->string tagg)))(Version->userList Version))
                
                 )
   )
 )

(define (Version->id Version)
  (car Version)
  )
(define (Version->author Version)
  (cadr Version)
  )
(define (Version->content Version)
  (cadr (cdddr (cdddr Version)))
  )

(define (Version->date Version)
  (cadddr Version)
  )

(define (Version->userList Version)
  (caddr Version)
  )





;funcion de prueba para hacer pruebas del funcionamiento...
;(filter odd? '(1 2 3 4 5))
;'(1 3 5)
(define (doble x)
   (* x 2))
(define (sera number)
  (if (> 3 number) 'yes 'no))

(define (UsarDoble x)(doble x))

(define printf (lambda X (apply format (cons #t X)) (newline)))
;como funciona string apped 
 (define (string-concat lst1 lst2)
     (string-append* (add-between (append lst1 lst2) " ")))