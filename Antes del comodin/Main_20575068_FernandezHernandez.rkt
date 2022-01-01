#lang racket
;archivos de TDA para usar en el programa
(provide (all-defined-out))
(require "Registro_20575068_FernandezHernandez.rkt")
(require "Create_20575068_FernandezHernandez.rkt")
(require "fecha_20575068_FernandezHernandez.rkt")
(require "anadir_20575068_FernandezHernandez.rkt")
(require "restorarVersion_20575068_FernandezHernandez.rkt")
(require "buscar_20575068_FernandezHernandez.rkt")
(require "string_20575068_FernandezHernandez.rkt")

; FUNCIONES PRINCIPALES!

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
(define (register doc Date username password )
  (if (Paradoc? doc) ;si -paradoc exite?
          doc ;devolver doc
          (comprobarRegistre doc Date username password);comprobar que los datos esten bien y de paso añadirlo
          ))

;------------------------------------------------------------------------------------------------------------------------------------------------------------------
;---login---
;Función que permite autenticar a un usuario registrado iniciar sesión y junto con ello permite la ejecución de comandos concretos dentro de la plataforma. Si la autenticación es válida (i.e., que el usuario existe). El retorno es una función correspondiente a la operación (operation) parcialmente evaluada con el parámetro paradigmadocs actualizado. La actualización de paradigmadocs incorpora al usuario autenticado en la sesión activa (fundamental para que el comando pueda funcionar). De lo contrario retorna solo operation
;dominio paradigmadocs X string X string X function
;recorrido paradigmadocs
;example (login paradigmadocs “user” “pass” create)

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
(define(UsarDoc doc)
   (lambda (CrearDate) ;una fecha
     (lambda (Create . nameDoc);nombre del doc
       (Crear doc CrearDate Create nameDoc)) ;la funcion crear colocara en labda todo y luego verificara con la funcion compruebaCreate finalmente lo añade
))

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
;---ADD---
;Función que permite añadir texto al final de la versión actual/activa del documento. La última versión del documento producto de cualquier cambio a través de esta función pasa a ser la versión activa. 
;Dom:paradigmadocs X int X date X String
;REC:paradigmadocs
(define (Add doc) 
  (lambda (AddDate);fecha
    (lambda (idDoc) ;id
      (if (sesion? doc);inicia session?
          (comprobarAdd AddDate idDoc Add doc ) ;comprobar y añadir
         ;si no fin
           (endParadoc (endSesion) (cdr doc))))))

;--------------------------------------------------------------------------------------------------------------------------------------------------
;---RESTORE Version ---
;Función que permite restaurar una versión anterior de un documento.
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


;-------------------------------------------------------------------------------------------------------------------------------------------------- 
;RevokeAllAccesses
;Función que permite al usuario revocar todos los
;accesos a sus documentos. Retorno final de la función es una versión actualizada de
;paradigmadocs donde se registra el cambio y se elimina la sesión activa del usuario en paradigmadocs.
;DOM paradigmadocs
;REC paradigmadocs

;FUNCION NO FUNCIONA OMITIMOS

(define (RevokeAllAccesses doc)
  (lambda (defined-Revoke)
    (if (sesion? doc)
     (defined-Revoke doc) ;no supo como hacer un remove funtion
     (endParadoc (endSesion) (cdr doc))
    )
    )
 )

;--------------------------------------------------------------------------------------------------------------------------------------------------
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



;--------------------------------------------------------------------------------------------------------------------------------------------------
;paradigmadocs->string:
;Función que recibe una plataforma del tipo
;paradigmadocs y entrega una representación del mismo como un string posible de
;visualizar de forma comprensible al usuario


(define (paradigmadocs->string  doc)
  ( juntarString doc) ;string es convertirlo a string y juntar todo
  )



;--------------------------------------------------------------------------------------------------------------------------------------------------
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