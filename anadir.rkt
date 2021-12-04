#lang racket
(provide (all-defined-out))
(require "Registro.rkt")
(require "Create.rkt")

;---------------------
;REPRESENTACION
;_______________________
;ingresar el doc , el id , fecha y luego lo que se agrega
;(paradigmadocs 2 (23 01 2001) "hola")


;_________________________
;PERTENENCIA
;_________________________

(define (comprobarAdd AddDate idDoc Add doc )
  (if (and (date? AddDate) (and (number? idDoc) (> idDoc 0))(string? Add))
          ;si la fecha es fecha xd y el id es un numero , y que el doc mayor a 0,y es un string lo que agregamos
           ((lambda (currentDoc) ;creacion currentDoc
              (actualParadoc(endSesion)
                 (Paradoc->users doc) ;Cadr del paradoc
                 (DocAct(Paradoc->Doc doc) (Doc->id currentDoc) (Doc->author currentDoc)AddDate ;creacion de nueva lista 
                                   (+ 1 (Doc->numAdd currentDoc)) ;AÑADIR +1
                                   (Doc->content currentDoc)) ;llevar al contenido
                 ;añadir
                 (añadirA doc currentDoc AddDate Add) ))) ;ahora hay que finalizar
           ;terminar al ser un if no cumplido.
           (endParadoc (endSesion) (cdr doc))))



;_________________________
;CONSTRUCTOR
;_________________________

;(define (DocAct docCreate idCreate author newLikeA newLikeF userList CreateDate newDate newFollow newAdd newContent)
  (define (DocAct docCreate idCreate author CreateDate newDate newAdd newContent)
    (if (null? docCreate)
      '() ;null
      (if (equal? idCreate (Create->id (car docCreate)))
          (cons (CreateNew idCreate
                              author
                              ;newLikeA
                              ;newLikeF
                              ;userList
                              CreateDate
                              newDate
                              ;newFollow
                              newAdd
                              newContent
                             )(cdr docCreate))
          (cons (car docCreate) (DocAct (cdr docCreate) idCreate author CreateDate newDate newAdd newContent))
          ;(cons (car docCreate) (DocAct (cdr docCreate) idCreate author newLikeA newLikeF userList CreateDate newDate newFollow newAdd newContent))
      )
    )
 )

;lista como es
(define ( comment)
  (list comment)
  )
;lista nueva a ponerle
(define (newNuevaLineatack comment)
  (list comment)
  )
;nueva linea con los datos
(define (newNuevaLinea idcomment authorUser userList datecomment commentContent)
  (list idcomment authorUser userList datecomment commentContent)
  )
;nueva lista añadir a nuevo
(define (newNuevaLineadoc  comment)
  (list comment)
  )

;________________________
;MODIFICADORES
;-----------------------
;añadir
(define (AddAddQ docdocGrande idNuevaLinea userActual userList date NuevaLineaContent idPost)
    (if (= idPost 1)
      (if (equal? '(()) (car docdocGrande))
          (cons (newNuevaLineatack (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent)) (cdr docdocGrande))
          (cons (append (car docdocGrande) (newNuevaLineadoc (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent))) (cdr docdocGrande))
       )
      (cons (car docdocGrande) (AddAddQ (cdr docdocGrande) idNuevaLinea userActual userList date NuevaLineaContent (- idPost 1)))
      )
 )

;creacion nueva
(define (CreateNew id author userList PostingDate lastActivity follow numcomment content)
  (list id author userList numcomment content)
 )


(define (añadirA doc currentDoc AddDate Add)
  (AddAddQ (Paradoc->AddQ doc)(+ 1 (Doc->numAdd currentDoc)) ;añadir doc nuevo 
                          (Paradoc->userLoginActual doc)
                          AddDate ;fecha
                          Add     ;añadir
                          (Doc->id currentDoc)))
;_________________________
;SELECTORES
;_________________________
;OBTENER ADDQ
(define (Paradoc->AddQ Paradoc)
  (cadddr Paradoc)
  )
;numero de añadirdo
(define (Doc->numAdd Post)
  (cadr (cdddr (cdddr Post)))
 )
;contenido
(define (Doc->content Post)
  (caddr (cdddr (cdddr Post))
  ))
;obtener paradoc
(define (Paradoc->Doc Paradoc)
  (caddr Paradoc)
  )
;obtener el id
(define (Doc->id Post)
  (car Post)
 )
;obtener el author
(define (Doc->author Post)
  (cadr Post)
 )
;obtener lo creado
(define (Create->id create)
  (car create)
  )




