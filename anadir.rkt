#lang racket
(provide (all-defined-out))



(define (Paradoc->AddQ SocialNetwork)
  (cadddr SocialNetwork)
  )

(define (Doc->numAdd Post)
  (cadr (cdddr (cdddr Post)))
 )
(define (Doc->content Post)
  (caddr (cdddr (cdddr Post))
  ))


(define (AddAddQ docdocGrande idNuevaLinea userActual userList date NuevaLineaContent idPost)
    (if (= idPost 1)
      (if (equal? '(()) (car docdocGrande))
          (cons (newNuevaLineatack (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent)) (cdr docdocGrande))
          (cons (append (car docdocGrande) (newNuevaLineadoc (newNuevaLinea idNuevaLinea userActual userList date  NuevaLineaContent))) (cdr docdocGrande))
       )
      (cons (car docdocGrande) (AddAddQ (cdr docdocGrande) idNuevaLinea userActual userList date NuevaLineaContent (- idPost 1)))
      )
 )

(define ( comment)
  (list comment)
  )
(define (newNuevaLineatack comment)
  (list comment)
  )

(define (newNuevaLinea idcomment authorUser userList datecomment commentContent)
  (list idcomment authorUser userList datecomment commentContent)
  )
(define (newNuevaLineadoc  comment)
  (list comment)
  )
(define (Paradoc->Doc SocialNetwork)
  (caddr SocialNetwork)
  )
(define (Doc->id Post)
  (car Post)
 )
(define (Doc->author Post)
  (cadr Post)
 )

(define (Create->id create)
  (car create)
  )

(define (CreateNew id author userList PostingDate lastActivity follow numcomment content)
  (list id author userList numcomment content)
 )

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

