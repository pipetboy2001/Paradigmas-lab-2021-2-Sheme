#lang racket
(provide (all-defined-out))

;---------------------
;REPRESENTACION
;_______________________
;ingresar el doc , el id , fecha y luego lo que se agrega
;(paradigmadocs 2 (23 01 2001) "hola")
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




