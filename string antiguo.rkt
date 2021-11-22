#lang racket

(define (stack->string doc)
  (string-append
   (loginActual->string (Paradoc->loginActual doc))
   (users->string (Paradoc->users doc))
   (Post-comment->string (Paradoc->Post doc) (Paradoc->commentQ doc))
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
     (Stack->string user->string users)
  )
 )
(define (Stack->string dataType->string doc)
  (if (null? doc)
      "No se encontró ningún registro. \n"
      (string-append (dataType->string (car doc)) "\n" (Stack->string dataType->string (cdr doc)))
  )
 )
(define (user->string doc)
  (string-append "Usuarios: " (user->username doc) "\n"
   )
 )
(define (Paradoc->users Paradoc)
  (cadr Paradoc)
  )
(define (Post-comment->string PostsStack commentStack)
  (if (null? PostsStack)
      ""
      (string-append
           (Post->string (car PostsStack))
           (comment->string (car commentStack))
            "\n"
            (Post-comment->string (cdr PostsStack) (cdr commentStack))
       )
  )
 )

(define (Post->string Post)
  (string-append "Post ID: " (number->string (Post->id Post)) "\n"
                 "Author: " (Post->author Post) "Views: " (number->string) "\n"
                 "userList: " ((lambda (tagg) (if (equal? '("") tagg) "Post no userList. \n" (userList->string tagg)))(Post->userList Post))
                 "Date: " (date->string (date->datePost (Post->dates Post)))
                 " '' " (Post->content Post) " ''\n"
                 "Last date: " (date->string (date->dateLast (Post->dates Post))))
                  "\n"
                 "comment: " (number->string (Post->numcomment Post)) "\n"
                 
                  "\n"
                 )

(define (date->datePost date)
  (car date)
  )
(define (Post->dates Post)
  (cadr (cdddr Post))
 )

(define (Post->content Post)
  (caddr (cdddr (cdddr Post))
  ))

(define (date->dateLast date)
  (cadr date)
  )
(define (Post->numcomment Post)
  (cadr (cdddr (cdddr Post)))
 )

(define (Post->id Post)
  (car Post)
 )
(define (Post->author Post)
  (cadr Post)
 )
(define (Post->userList Post)
  (cadddr  Post)
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
(define (Paradoc->Post Paradoc)
  (caddr Paradoc)
  )

(define (Paradoc->commentQ Paradoc)
  (cadddr Paradoc)
  )

(define (comment->string comment)
  (if (null? comment)
         "Post without comment. \n"
         (string-append "comment id: " (number->string (comment->id comment)) " Author: " (comment->author comment) "\n"
                 "  '' " (comment->content comment) " ''\n"
                 "\t Enviado el: " (date->string (comment->date comment))
                 "\t userList " ((lambda (tagg) (if (equal? '("") tagg) "No found userList. \n"
                                                    (userList->string tagg)))(comment->userList comment))
                
                 )
   )
 )

(define (comment->id comment)
  (car comment)
  )
(define (comment->author comment)
  (cadr comment)
  )
(define (comment->content comment)
  (cadr (cdddr (cdddr comment)))
  )

(define (comment->date comment)
  (cadddr comment)
  )

(define (comment->userList comment)
  (caddr comment)
  )

(define (Paradoc)
  '(() () () ())
  )


